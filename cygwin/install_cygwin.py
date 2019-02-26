#!/usr/bin/env python
import os
import subprocess
import logging
import yaml
import re
import requests
import ctypes
import gzip
import traceback
import sys
from argparse import RawDescriptionHelpFormatter
from datetime import datetime, timedelta
from tarfile import TarFile, ReadError
from time import strftime
from e3.main import Main
from e3.fs import mkdir, ls, cp, rm, mv
from e3.env import Env
from e3.os.process import Run

# Help strings
DESCRIPTION = """
The script allows to do fresh install of cygwin, update it, add/remove
packages,...

To get more information on each command do:

    $ python ./cygwin_install.py <command> --help

To do a quick new install do:

    $ python ./cygwin_install.py install-all
"""

INSTALLALL_DESCRIPTION = """
Perform a clean new install of Cygwin. By default the script installs the
64-bit version of Cygwin which is known to have less rebase issues.

If a previous install is present in c:\\cygwin then the installation will
be moved to c:\\cygwin-prev-<current_date>. This allows to restore easily
the previous installation in case the new one fails.

By default all downloaded packages will be cached in c:\\cygwin_packages.

The list of required packages during a fresh installation can be found along
with the current script in cygwin_package.yaml. Additional package can after
the installation be added using the install command (do: cygwin_install.py
install --help for more information).

The typical command to do a 64-bit installation will be:

    $ python ./cygwin_install.py install-all

To perform a 32bits installation:

    $ python ./cygwin_install.py --version=x86 install-all

"""

UPDATE_DESCRIPTION = """
Perform an update with latest version of currently installed packages

"""

INSTALL_DESCRIPTION = """
Install a package and all its dependencies.

Package names can be found using the search command (do: cygwin_install.py
search --help for more information).

The option --dry-run will show the list of effective packages that will be
installed without performing the installation.
"""

UNINSTALL_DESCRIPTION = """
Uninstall a package and all packages depending on that package.

Package names can be found using the search command (do: cygwin_install.py
search --help for more information).

The option --dry-run will show the list of effective packages that will be
uninstalled without performing the uninstallation.
"""

SEARCH_DESCRIPTION = """
Search in the package database.

If --regex is passed the command will output the list of packages with
name, short description or long description matching the regexp.

If --regex is not passed then command will output the complete list of
packages along with their short description.
"""

UPDATEMIRROR_DESCRIPTION = """
Update Cygwin mirror used.

When doing the first installation, the installer will try to find the fastest
mirror automatically. The mirror used is then kept in the cache. In case
the mirror becomes invalid you can restart the search for the best mirror
using that command.
"""

# Directory containing this script
setup_dir = os.path.abspath(os.path.dirname(__file__))

# The following conditional import ensures that at least --help works
# on non windows platforms
if sys.platform == 'win32':
    import win32api
    from win32security import LookupAccountName, ACL, GetFileSecurity, \
        DACL_SECURITY_INFORMATION, GROUP_SECURITY_INFORMATION, \
        OWNER_SECURITY_INFORMATION, ACL_REVISION, SetFileSecurity, GetBinarySid
    from ntsecuritycon import CONTAINER_INHERIT_ACE, OBJECT_INHERIT_ACE, \
        INHERIT_ONLY_ACE, STANDARD_RIGHTS_ALL, FILE_GENERIC_READ, \
        FILE_GENERIC_WRITE, FILE_GENERIC_EXECUTE, FILE_DELETE_CHILD, \
        STANDARD_RIGHTS_READ, FILE_READ_ATTRIBUTES, READ_CONTROL, \
        WRITE_DAC, WRITE_OWNER

    # Retrieve some useful SIDs needed to set correct ACLs
    USER, _, _ = LookupAccountName("", win32api.GetUserName())
    print 'User: %s' % win32api.GetUserName()
    EVERYONE = GetBinarySid("S-1-1-0")
    GROUP = GetBinarySid("S-1-5-32-545")
    CREATOR_USER = GetBinarySid("S-1-3-0")
    CREATOR_GROUP = GetBinarySid("S-1-3-1")
    ADMINS = GetBinarySid("S-1-5-32-544")

# List of mirrors to avoid (speeds up detection of fastest Cygwin mirror)
MIRROR_BLACK_LIST = ['http://gd.tuwien.ac.at/gnu/cygwin/']


class CygwinMirror(object):
    """CygwinMirror instance."""

    def __init__(self, download_dir, url=None, reset=False):
        """Initialize cygwin mirror.

        :param download_dir: path in which last mirror used is stored
        :type download_dir: str
        :param url: selected url. If None the class tries to find the fastest
            mirror.
        :type url: str | None
        :param reset: if True ignore last used mirror
        :type reset: bool
        """
        self.log = logging.getLogger('mirrors')
        assert os.path.isdir(download_dir)
        self.mirror_path = os.path.join(download_dir, 'last_mirror.txt')
        self.update_mirror_list()

        if url is not None:
            self.url = url
        else:
            self.get_last_used_mirror()
            if reset or self.url is None or self.url not in self.mirror_list:
                self.find_fastest_mirror()
        self.set_last_used_mirror()

    def get_last_used_mirror(self):
        if os.path.isfile(self.mirror_path):
            with open(self.mirror_path, 'rb') as fd:
                self.url = fd.read().strip()
        else:
            self.url = None

    def set_last_used_mirror(self):
        with open(self.mirror_path, 'wb') as fd:
            fd.write(self.url)

    def check(self, url, max_time=None):
        """Check cygwin mirror and time to fetch setup.ini.

        :param url: url to check
        :type url: str
        :param max_time: timeout for getting setup.ini
        :type max_time: datetime.timedelta
        :return: amount of time necessary to fetch setup.ini or None
        :rtype: datetime.timedelta | None
        """
        start = datetime.now()
        if max_time is None:
            timeout = 20
        else:
            timeout = max_time.total_seconds()

        try:
            r = requests.get(url + 'x86/setup.ini',
                             stream=True,
                             timeout=timeout)

            for chunk in r.iter_content(1024):
                if max_time is not None and \
                        datetime.now() - start > max_time:
                    return None
        except Exception:
            return None
        return datetime.now() - start

    def update_mirror_list(self):
        """Fetch the list of mirrors."""
        self.mirror_list = requests.get("http://cygwin.com/mirrors.lst",
                                        stream=True).text.splitlines()
        # We do not handle currently ftp mirrors
        self.mirror_list = [m for m in self.mirror_list
                            if m.startswith('http://')]
        self.mirror_list = [k.split(';') for k in self.mirror_list]
        self.mirror_list = {k[0]: {'url': k[0],
                                   'name:': k[1],
                                   'region': k[2],
                                   'country': k[3]}
                            for k in self.mirror_list}
        self.log.info('Found %s servers' % len(self.mirror_list))

    def find_fastest_mirror(self):
        """Return the fastest cygwin mirror.

        :param region: reduce search on that region
        :type region: str | None
        :param country: reduce search on that country
        :type country: str | None
        """

        fastest_server = None
        servers = {}

        for url, prop in self.mirror_list.iteritems():
            # Fastest servers are in europe and united states
            if prop['region'] not in ('Europe', 'United States'):
                continue

            # Ignore blacklisted mirrors that are known to be very slow
            if url in MIRROR_BLACK_LIST:
                continue

            servers[url] = self.check(url, None if fastest_server is None
                                      else servers[fastest_server])

            if fastest_server is None and servers[url] is not None:
                self.log.info('fastest server for now: %s (%s)' %
                              (url, servers[url]))
                fastest_server = url
            elif servers[url] is not None and \
                    servers[url] < servers[fastest_server]:
                self.log.info('fastest server for now: %s (%s)' %
                              (url, servers[url]))
                fastest_server = url
                if servers[url] < timedelta(seconds=1):
                    break
            else:
                self.log.debug('%s is slower (%s)' % (url, servers[url]))

        logging.info('fastest server is: %s' % fastest_server)
        self.url = fastest_server
        return fastest_server


class CygwinSetupIni(object):
    """setup.ini and package loader manager."""

    def __init__(self, mirror, download_dir, version='x86'):
        self.download_dir = download_dir
        assert os.path.isdir(self.download_dir), \
            'invalid download dir %s' % self.download_dir
        self.mirror = mirror
        self.setup_ini = os.path.join(self.download_dir, 'setup.ini')
        self.log = logging.getLogger('setup_ini')
        self.version = version
        self.packages = {}
        self.download()
        self.load()

    def download(self):
        answer = requests.get(self.mirror.url + self.version + '/setup.ini')
        answer.encoding = 'utf-8'
        p = answer.text
        with open(self.setup_ini, 'wb') as fd:
            fd.write(p.encode('utf-8'))

    def compute_base(self):
        """Get minimal package list"""
        return [m for m in self.packages
                if 'Base' in self.packages[m]['category']]

    def download_package(self, name):
        """Download a cygwin package"""
        path = self.packages[name]['install'][0]
        if os.path.isfile(os.path.join(self.download_dir, path)):
            return

        answer = requests.get(self.mirror.url + path, stream=True)
        mkdir(os.path.join(self.download_dir, os.path.dirname(path)))
        with open(os.path.join(self.download_dir, path), 'wb') as fd:
            fd.write(answer.raw.read())

    def package_path(self, name):
        path = self.packages[name]['install'][0]
        return os.path.abspath(os.path.join(self.download_dir, path))

    def package_list(self, modules):
        result = set(self.compute_base())
        if modules:
            result |= set(modules)

        length = 0

        while True:
            tmp = set()
            for m in result:
                tmp |= set(self.packages[m].get('requires', []))
            result |= tmp

            if length == len(result):
                break
            else:
                length = len(result)
        return result

    def remove_package_list(self, p):
        result = set([p])
        length = 1

        while True:
            for m in self.packages:
                if set(self.packages[m].get('requires', [])) & result:
                    result.add(m)
            if length == len(result):
                break
            else:
                length = len(result)
        return result

    def load(self):
        with open(self.setup_ini, 'rb') as fd:
            content = fd.read()

        in_module = None
        for l in content.splitlines():
            if l.startswith('@ '):
                in_module = l[2:]
                self.packages[in_module] = {}
            else:
                if in_module is None:
                    continue

                if l.strip() in ("[prev]", "[test]"):
                    in_module = None
                    continue

                m = re.match(r'([a-z-]+): (.*)', l)
                if m is not None:
                    field = m.group(1)
                    value = m.group(2)
                    if value.startswith('"'):
                        self.packages[in_module][field] = value
                    else:
                        self.packages[in_module][field] = value.split()
                else:
                    self.packages[in_module][field] += l


class CygwinDB(object):
    def __init__(self, root_dir, ini):
        self.db_path = os.path.join(root_dir, 'etc', 'setup', 'installed.db')
        self.ini = ini
        self.installed = {}
        if os.path.isfile(self.db_path):
            self.load()

    def load(self):
        with open(self.db_path, 'rb') as fd:
            content = fd.read().splitlines()
        for l in content:
            if l.startswith('INSTALLED.DB'):
                continue
            package, package_file, _ = l.split()
            self.installed[package] = package_file

    def dump(self):
        with open(self.db_path, 'wb') as db_fd:
            db_fd.write('INSTALLED.DB 2\n')
            for key, basepath in self.installed.iteritems():
                db_fd.write("%s %s 0\n" % (key, basepath))

    def __contains__(self, p):
        return p in self.installed

    def up_to_date(self, p):
        return p in self.installed and \
            self.installed[p] == os.path.basename(self.ini.package_path(p))

    def __isub__(self, p):
        if p in self.installed:
            del self.installed[p]
        return self

    def __iadd__(self, p):
        self.installed[p] = os.path.basename(self.ini.package_path(p))
        return self


class Cygwin(object):

    def __init__(self, ini, reset=False):

        self.root_dir = 'c:\\cygwin'
        self.prev_dir = None
        if reset:
            # Save previous installation
            if os.path.isdir(self.root_dir):
                self.prev_dir = self.root_dir + \
                    '-prev-' + strftime('%Y%m%d-%H%M')
                logging.warning('move previous installation to %s-prev'
                                % self.root_dir)
                mv(self.root_dir, self.prev_dir)

        self.ini = ini
        # with the installation logs
        self.tmpdir = self.ini.download_dir

        # Ensure our tmp dir is used
        os.environ['TMP'] = self.ini.download_dir
        os.environ['TMPDIR'] = self.ini.download_dir

        # User required packages
        self.to_be_removed = set()
        self.to_be_installed = set()

        # Initialize db
        self.db = CygwinDB(self.root_dir, self.ini)
        self.required = set([k for k in self.db.installed])

    def add_package(self, p):
        if p in self.ini.packages:
            before = set([k for k in self.required])
            self.required.add(p)
            self.required = set(self.ini.package_list(self.required))
            added = self.required - before
            for p in added:
                logging.info('adding: %s' % p)
        else:
            logging.error('%s cannot be added (does not exists)' % p)

    def remove_package(self, p):
        removed_packages = set(self.ini.remove_package_list(p))
        result = self.required & removed_packages
        logging.info('will remove %s' % ", ".join(result))
        self.to_be_removed |= result

    def resolve_packages(self):
        # Get the full closure of the packages

        for p in self.required:
            if p in self.db:
                if not self.db.up_to_date(p):
                    self.to_be_removed.add(p)
                    self.to_be_installed.add(p)
            else:
                self.to_be_installed.add(p)

    def install_all(self):
        """Perform a full Cygwin installation."""
        self.resolve_packages()
        mkdir(self.root_dir)
        try:
            self.cygwin_chmod(self.root_dir, 0755)
        except Exception:
            pass
        self.download()
        self.install()
        self.update_mount_points()
        self.adjust_cygwin_dll()
        self.install_shortcuts()

        if self.prev_dir:
            # If we have a previous installation try to retrieve
            # previous machine host keys.
            self.cygcmd('cp', '-p',
                        self.prev_dir.replace('\\', '/') + '/etc/ssh_host_*',
                        '/etc')

    def cygpath(self, *args):
        return os.path.join(self.root_dir, *args)

    def cyglog(self, name):
        return os.path.join(self.ini.download_dir, name)

    def cygcmd(self, *args):
        Run([self.cygpath('bin', args[0] + '.exe')] + list(args[1:]))

    def download(self, package_list=None):
        """Download required cygwin packages"""

        package_number = len(self.to_be_installed)

        for index, p in enumerate(self.to_be_installed):
            logging.info('Downloading package (%s/%s): %s' %
                         (index + 1, package_number, p))
            self.ini.download_package(p)

    def get_target_path(self, path):
        if path.startswith('usr/bin'):
            result = path.replace('usr/bin/', 'bin/', 1)
        elif path.startswith('usr/lib'):
            result = path.replace('usr/lib/', 'lib/', 1)
        else:
            result = path

        for c in ('"', '*', ':', '<', '>', '?', '|'):
            result = unicode(result).replace(c, unichr(0xf000 | ord(c)))
        return result

    def unpack(self, tar_archive):
        """Unpack a cygwin archive

        Note that the function creates cygwin links whenever it is necessary
        """

        os.chdir(self.root_dir)
        file_list = []
        try:
            with TarFile(tar_archive) as tar_fd:
                for next_file in tar_fd.getmembers():
                    path = self.get_target_path(next_file.name)

                    if next_file.issym():
                        target_path = self.get_target_path(next_file.linkname)
                        logging.debug("create %s -> %s" % (path, target_path))
                        if os.path.isfile(path):
                            os.unlink(path)
                        mkdir(os.path.dirname(path), 0777)
                        with open(path, 'wb') as path_fd:
                            path_fd.write("!<symlink>%s\0" % target_path)
                        ctypes.windll.kernel32.SetFileAttributesW(
                            unicode(path),
                            0x4)
                        file_list.append(next_file.name)
                    elif next_file.isdir():
                        logging.debug("create dir %s" % path)
                        mkdir(path, 0777)
                        file_list.append(next_file.name + '/')
                        self.cygwin_chmod(path, next_file.mode)
                    else:
                        logging.debug("create %s" % path)
                        mkdir(os.path.dirname(path), 0777)
                        rm(path)
                        path_fd = tar_fd.extractfile(next_file)
                        with open(path, 'wb') as target_fd:
                            target_fd.write(path_fd.read())
                        path_fd.close()
                        self.cygwin_chmod(path, next_file.mode)
                        file_list.append(next_file.name)
        except ReadError as e:
            if str(e) != 'empty file':
                raise

        return file_list

    def cygwin_chmod(self, f, mode):
        def add_dacl(dacl, attrs, who, children_only=False):
            if children_only:
                dacl.AddAccessAllowedAceEx(
                    ACL_REVISION,
                    CONTAINER_INHERIT_ACE |
                    OBJECT_INHERIT_ACE |
                    INHERIT_ONLY_ACE,
                    attrs,
                    who)
            else:
                dacl.AddAccessAllowedAce(ACL_REVISION,
                                         attrs,
                                         who)

        # First user rights
        dacl = ACL()

        u_attrs = STANDARD_RIGHTS_ALL | FILE_GENERIC_READ | FILE_GENERIC_WRITE
        if mode & 0100:
            u_attrs |= FILE_GENERIC_EXECUTE
        if (mode & 0300) == 0300:
            u_attrs |= FILE_DELETE_CHILD

        g_attrs = STANDARD_RIGHTS_READ | FILE_READ_ATTRIBUTES
        if mode & 0040:
            g_attrs |= FILE_GENERIC_READ
        if mode & 0020:
            g_attrs |= FILE_GENERIC_WRITE
        if mode & 0010:
            g_attrs |= FILE_GENERIC_EXECUTE
        if (mode & 01030) == 00030:
            g_attrs |= FILE_DELETE_CHILD

        # others
        o_attrs = STANDARD_RIGHTS_READ | FILE_READ_ATTRIBUTES
        if mode & 0004:
            o_attrs |= FILE_GENERIC_READ
        if mode & 0002:
            o_attrs |= FILE_GENERIC_WRITE
        if mode & 0001:
            o_attrs |= FILE_GENERIC_EXECUTE
        if (mode & 01003) == 00003:
            o_attrs |= FILE_DELETE_CHILD

        a_attrs = READ_CONTROL | WRITE_DAC | WRITE_OWNER

        add_dacl(dacl, u_attrs, USER)
        add_dacl(dacl, a_attrs, ADMINS)
        add_dacl(dacl, g_attrs, GROUP)
        add_dacl(dacl, o_attrs, EVERYONE)

        if os.path.isdir(f):
            if mode & 01000:
                g_attrs = STANDARD_RIGHTS_READ | FILE_READ_ATTRIBUTES
                if mode & 0040:
                    g_attrs |= FILE_GENERIC_READ
                if mode & 0010:
                    g_attrs |= FILE_GENERIC_EXECUTE
                o_attrs = STANDARD_RIGHTS_READ | FILE_READ_ATTRIBUTES
                if mode & 0004:
                    o_attrs |= FILE_GENERIC_READ
                if mode & 0001:
                    o_attrs |= FILE_GENERIC_EXECUTE
            add_dacl(dacl, u_attrs, CREATOR_USER, True)
            add_dacl(dacl, g_attrs, CREATOR_GROUP, True)
            add_dacl(dacl, o_attrs, EVERYONE, True)

        sd = GetFileSecurity(f, DACL_SECURITY_INFORMATION)
        sd.SetSecurityDescriptorDacl(1, dacl, 0)
        sd.SetSecurityDescriptorOwner(USER, 0)
        sd.SetSecurityDescriptorGroup(GROUP, 0)
        try:
            SetFileSecurity(f,
                            DACL_SECURITY_INFORMATION |
                            GROUP_SECURITY_INFORMATION |
                            OWNER_SECURITY_INFORMATION,
                            sd)
        except Exception:
            logging.error('cannot set permissions on %s' % f)
            raise

    def install(self):
        os.chdir(self.root_dir)

        for d in (("/bin", 0755),
                  ("/dev", 0755),
                  ("/dev/mqueue", 01777),
                  ("/dev/shm", 01777),
                  ("/etc", 0755),
                  ("/etc/fstab.d", 01777),
                  ("/lib", 0755),
                  ("/tmp", 01777),
                  ("/usr", 0755),
                  ("/usr/bin", 0755),
                  ("/usr/lib", 0755),
                  ("/usr/local", 0755),
                  ("/usr/local/bin", 0755),
                  ("/usr/local/etc", 0755),
                  ("/usr/local/lib", 0755),
                  ("/usr/src", 0755),
                  ("/usr/tmp", 01777),
                  ("/var", 0755),
                  ("/var/log", 01777),
                  ("/var/run", 01777),
                  ("/var/tmp", 01777),
                  ("/etc/setup", 0755)):
            mkdir(self.root_dir + d[0])
            self.cygwin_chmod(self.root_dir + d[0], d[1])
        # Ensure /var/empty is created. When updating an installation
        # the /var/empty security might have been tighten by cyg_server
        # account (sshd) and thus chmod might fail. Just ignore the
        # error
        try:
            mkdir(self.root_dir + '/var/empty')
            self.cygwin_chmod(self.root_dir + '/var/empty', 0755)
        except Exception:
            pass

        for d in (('c:/home', 01777),
                  ('c:/tmp', 01777),
                  ('c:/tmp/trash', 01777)):
            mkdir(d[0])
            self.cygwin_chmod(d[0], d[1]),

        package_number = len(self.to_be_removed)
        for index, p in enumerate(self.to_be_removed):
            logging.info('Remove package (%s/%s): %s' %
                         (index + 1, package_number, p))
            self.uninstall_package(p)

        package_number = len(self.to_be_installed)
        for index, p in enumerate(self.to_be_installed):
            logging.info('Installing packages (%s/%s): %s' %
                         (index + 1, package_number, p))
            self.install_package(p)
        self.run_postinstall()
        self.db.dump()

    def uninstall_package(self, p):
        if p not in self.db:
            return
        os.chdir(self.root_dir)
        manifest_path = os.path.join(self.root_dir, 'etc', 'setup',
                                     p + '.lst.gz')
        with gzip.open(manifest_path, 'rb') as list_fd:
            content = list_fd.read()
        file_list = [k.strip() for k in content.splitlines()]
        file_list.reverse()

        for path in file_list:
            logging.info('remove %s' % path)
            fullpath = os.path.join(self.root_dir, self.get_target_path(path))
            if os.path.isdir(fullpath):
                try:
                    os.rmdir(fullpath)
                except Exception:
                    pass
            else:
                rm(fullpath)
        rm(manifest_path)
        self.db -= p

    def install_package(self, p):
        if self.db.up_to_date(p):
            return
        if p in self.db:
            # p is installed but not up-to-date. We need first to uninstall it
            self.uninstall_package(p)

        package_path = self.ini.package_path(p)
        base_path = os.path.basename(package_path)
        tar_archive = base_path.rsplit('.', 1)[0]
        manifest_path = os.path.join(self.root_dir, 'etc', 'setup',
                                     p + '.lst.gz')

        with open(self.cyglog('install.log'), 'wb') as fd:
            os.chdir(self.root_dir)
            subprocess.check_call(
                    [os.path.join(setup_dir, '7z.exe'),
                     'x', '-y', package_path],
                    stdout=fd)
            self.unpack(tar_archive)
            file_list = self.unpack(tar_archive)
            with gzip.open(manifest_path, 'wb') as list_fd:
                list_fd.write('\n'.join(file_list) + '\n')
            rm(tar_archive)

        self.db += p

    def run_postinstall(self):
        """Run cygwin postinstall scripts"""
        with open(self.cyglog('postinstall.log'), 'wb') as fd:
            os.chdir(self.root_dir)

            # Compute the list of postinstall scripts
            pscripts = []
            for ext in ('.sh', '.bat', '.cmd', '.dash'):
                pscripts += ls('etc/postinstall/*' + ext)
            pscripts.sort()

            # Set some env variables needed by the postinstall scripts
            os.environ['SHELL'] = '/bin/bash'
            os.environ['CYGWINROOT'] = self.root_dir
            os.environ['TERM'] = 'dump'
            for p in (('usr', 'bin'), ('bin', ), ('usr', 'sbin'), ('sbin', )):
                os.environ['PATH'] = os.path.join(self.root_dir, *p) + ';' + \
                    os.environ['PATH']

            # run postinstall scripts
            for index, ps in enumerate(pscripts):
                logging.info('Run postinstall (%s/%s) %s' %
                             (index, len(pscripts), os.path.basename(ps)))
                fd.write('run %s\n' % ps)

                if ps.endswith('.dash'):
                    Run([os.path.join(self.root_dir, 'bin', 'dash.exe'), ps],
                        output=fd)
                elif ps.endswith('.sh'):
                    Run([os.path.join(self.root_dir, 'bin', 'bash.exe'),
                         '--norc', '--noprofile', ps], output=fd)
                mv(ps, ps + '.done')

    def should_be_mounted(self, path):
        # the following dirs should not be added to /etc/fstab
        root = 'c:\\'
        not_mounted_dirs = (root + 'system volume information',
                            root + 'perflogs',
                            root + 'bin',
                            root + 'cygdrive',
                            root + 'dev',
                            root + 'etc',
                            root + 'lib',
                            root + 'sbin',
                            root + 'usr',
                            root + 'var')

        # filter also $recycle bin or others hidden dirs
        if path.startswith('c:\\$') or path in not_mounted_dirs:
            return False
        else:
            return True

    def update_mount_points(self):
        logging.info('update moint points')
        fd = open(os.path.join(self.root_dir, "etc", "fstab"), "w")
        mount_list = ls('c:\*')

        mount_list = [k for k in mount_list if
                      os.path.isdir(k) and self.should_be_mounted(k.lower())]
        mount_list = [k.replace('\\', '/').replace(' ', '\\040')[2:]
                      for k in mount_list]

        fd.write("none /cygdrive cygdrive binary,posix=0,user 0 0\n")
        for k in mount_list:
            if k.startswith('/' + Env().host.machine) or \
                    k == '/gnatmail' or k == '/it':
                fd.write("c:%s %s ntfs binary,posix=0,noacl 0 0\n" % (k, k))
            else:
                fd.write("c:%s %s ntfs binary,posix=0 0 0\n" % (k, k))
        fd.close()

    def add_mountpoint(self, path):
        fd = open(os.path.join(self.root_dir, "etc", "fstab"), 'a')
        k = path.split(':', 1)[1].replace('\\', '/')
        fd.write('c:%s %s ntfs binary,posix=0 0 0\n' % (k, k))
        fd.close()
        subprocess.check_call(['c:/cygwin/bin/mount.exe', '-a'])

    def install_shortcuts(self):
        """Create shortcuts and additional directories"""
        logging.info('Install shortcuts')
        mkdir('c:/cygwin/mingw')
        mkdir('c:/cygwin/mingw/include')
        cp(os.path.join(setup_dir, 'shortcuts', '*.lnk'),
           os.path.join(os.environ['USERPROFILE'], 'desktop'))

    def adjust_cygwin_dll(self):
        """Launch rebasing and peflagsall on all Cygwin"""
        logging.info("Run rebaseall and peflagsall (IGNORE WARNINGS)")
        os.chdir(os.path.join(self.root_dir, 'bin'))

        subprocess.check_call(["c:/cygwin/bin/ash.exe", "./rebaseall"])
        subprocess.check_call(["c:/cygwin/bin/ash.exe", "./peflagsall"])
        logging.info("rebase passed")


def mirror_cmd(options):
    CygwinMirror(options.download_dir,
                 reset=True)


def setup_ini_cmd(options):
    cm = CygwinMirror(options.download_dir)
    si = CygwinSetupIni(cm, options.download_dir, version=options.version)
    if options.regex:
        pkgs = [k for k in si.packages if
                re.search(options.regex, k, flags=re.I) or
                re.search(options.regex,
                          si.packages[k].get('ldesc', ''), flags=re.I) or
                re.search(options.regex,
                          si.packages[k].get('sdesc', ''), flags=re.I)]
    else:
        pkgs = [k for k in si.packages]
    for p in pkgs:
        logging.info('%-32s | %s' % (p, si.packages[p].get('sdesc', '')))


def install_all_cmd(options):
    cm = CygwinMirror(options.download_dir)
    si = CygwinSetupIni(cm, options.download_dir, options.version)
    c = Cygwin(si, reset=True)
    with open(os.path.join(setup_dir, 'cygwin_packages.yaml'), 'rb') as fd:
        packages = yaml.load(fd)
        for p in packages:
            c.add_package(p)
    if options.with_x11:
        c.add_package('xorg-server')
        c.add_package('xinit')
    c.install_all()


def update_cmd(options):
    cm = CygwinMirror(options.download_dir)
    si = CygwinSetupIni(cm, options.download_dir, options.version)
    c = Cygwin(si)
    c.install_all()


def add_package_cmd(options):
    cm = CygwinMirror(options.download_dir)
    si = CygwinSetupIni(cm, options.download_dir, options.version)
    c = Cygwin(si)
    c.add_package(options.package)
    if not options.dry_run:
        c.install_all()


def remove_package_cmd(options):
    cm = CygwinMirror(options.download_dir)
    si = CygwinSetupIni(cm, options.download_dir, options.version)
    c = Cygwin(si)
    c.remove_package(options.package)
    if not options.dry_run:
        c.install_all()


if __name__ == '__main__':
    m = Main()
    parser = m.argument_parser
    parser.description = DESCRIPTION

    # Ensure that if we have already an installation then we used the same
    # version (32bits or 64bits)
    if os.path.isfile('c:/cygwin/etc/rebase.db.x86_64'):
        default_version = 'x86_64'
    elif os.path.isfile('c:/cygwin/etc/rebase.db.x86'):
        default_version = 'x86'
    else:
        default_version = 'x86_64'

    parser.add_argument(
        "--download-dir",
        default='c:\\cygwin_packages',
        help="Set download directory (default: c:\\cygwin_packages)")
    parser.add_argument(
        "--version", default=default_version,
        help="Select version: x86 or x86_64 (default: %s)" % default_version)
    subparsers = parser.add_subparsers()

    # Add install-all command
    subparser = subparsers.add_parser(
        "install-all",
        help="Do a complete installation",
        formatter_class=RawDescriptionHelpFormatter,
        description=INSTALLALL_DESCRIPTION)
    subparser.add_argument('--with-x11',
                           help='Install X11 environment',
                           action="store_true", default=False)
    subparser.set_defaults(cmd=install_all_cmd)

    # Add update method
    subparser = subparsers.add_parser(
        "update",
        help="Update installation",
        formatter_class=RawDescriptionHelpFormatter,
        description=UPDATE_DESCRIPTION)
    subparser.set_defaults(cmd=update_cmd)

    # Add add package method
    subparser = subparsers.add_parser(
        "install",
        help="Add a package",
        formatter_class=RawDescriptionHelpFormatter,
        description=INSTALL_DESCRIPTION)
    subparser.add_argument('package', help="Package name")
    subparser.add_argument('--dry-run', action="store_true", default=False,
                           help="Do dry run")
    subparser.set_defaults(cmd=add_package_cmd)

    # Add uninstall package method
    subparser = subparsers.add_parser(
        "uninstall",
        help="Remove a package",
        formatter_class=RawDescriptionHelpFormatter,
        description=UNINSTALL_DESCRIPTION)
    subparser.add_argument('package', help="Package name")
    subparser.add_argument('--dry-run', action="store_true", default=False,
                           help="Do dry run")
    subparser.set_defaults(cmd=remove_package_cmd)

    # Add setup related functions
    subparser = subparsers.add_parser(
        "search",
        help="Search package",
        formatter_class=RawDescriptionHelpFormatter,
        description=SEARCH_DESCRIPTION)
    subparser.add_argument('--regex',
                           default=None,
                           help="Regexp to match")
    subparser.set_defaults(cmd=setup_ini_cmd)

    # Add mirror related command
    subparser = subparsers.add_parser(
        "update-mirror",
        help="Find fastest mirror",
        formatter_class=RawDescriptionHelpFormatter,
        description=UPDATEMIRROR_DESCRIPTION)
    subparser.set_defaults(cmd=mirror_cmd)

    m.parse_args()
    requests_log = logging.getLogger("requests")
    requests_log.setLevel(logging.WARNING)
    mkdir(m.args.download_dir)
    try:
        result = m.args.cmd(m.args)
    except Exception:
        logging.error(traceback.format_exc())
        logging.error('Installation failed')
        result = 1
    raw_input("Press Enter to continue...")
    sys.exit(result)
