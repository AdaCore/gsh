from e3.fs import mkdir
from e3.os.process import Run
from e3.os.fs import df
from e3.testsuite.driver import TestDriver
from e3.testsuite.process import check_call
from e3.testsuite.result import TestStatus
import os
import logging
import traceback


# Root directory of respectively the testsuite and the gsh
# repository.
TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__)))
GSH_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)

DEFAULT_TIMEOUT = 5 * 60  # 5 minutes


def make_gsh(work_dir, gcov=False, recompile=True):
    """Build gsh with or without gcov instrumentation.

    :param work_dir: working directory. gsh is built in `build` subdir
        and installed in `install` subdir
    :type work_dir: str
    :param gcov: if False then build gcov in PROD mode, otherwise
        build it with gcov instrumentation in DEBUG mode
    :type gcov: bool
    :return: a triplet (project path, source path, object path)
    :rtype: (str, str, str)
    :raise AssertError: in case compilation of installation fails
    """
    logging.info('Compiling projects (gcov=%s)' % gcov)

    # Create build tree structure
    build_dir = os.path.join(work_dir, 'build')
    install_dir = os.path.join(work_dir, 'install')
    os.environ['GPR_PROJECT_PATH'] = os.path.join(install_dir, 'share', 'gpr')
    mkdir(build_dir)
    mkdir(install_dir)

    # Compute make invocation
    gprbuild_opts = []
    if gcov:
        gprbuild_opts = ['-cargs', '-fprofile-arcs', '-ftest-coverage',
                         '-largs', '-lgcov',
                         '-gargs']

    if recompile:
        for project in ('c', 'os', 'gsh'):
            logging.info('Compiling project %s', project)
            obj_dir = os.path.join(build_dir, project)
            mkdir(obj_dir)

            # Build & Install
            p = Run(['gprbuild', '--relocate-build-tree', '-p', '-P',
                     os.path.join(GSH_ROOT_DIR, project,
                                  '%s.gpr' % project)] + gprbuild_opts,
                    cwd=obj_dir, timeout=DEFAULT_TIMEOUT,
                    output=None)

            assert p.status == 0, \
                "%s installation failed:\n%s" % (project, p.out)

            p = Run(['gprinstall', '-p', '-f', '--prefix=%s' % install_dir,
                     '--relocate-build-tree',
                     '-P', os.path.join(GSH_ROOT_DIR, project,
                                        '%s.gpr' % project)],
                    cwd=obj_dir, timeout=DEFAULT_TIMEOUT)
            assert p.status == 0, \
                "%s installation failed:\n%s" % (project, p.out)

        # Build & Install
        p = Run(['gprbuild', '--relocate-build-tree', '-p', '-P',
                 os.path.join(GSH_ROOT_DIR,
                              project, '%s.gpr' % project)] +
                gprbuild_opts,
                cwd=obj_dir, timeout=DEFAULT_TIMEOUT,
                output=None)

        assert p.status == 0, "%s installation failed:\n%s" % (project, p.out)

        p = Run(['gprinstall', '-p', '-f', '--prefix=%s' % install_dir,
                 '--relocate-build-tree',
                 '-P', os.path.join(GSH_ROOT_DIR,
                                    project, '%s.gpr' % project)],
                cwd=obj_dir, timeout=DEFAULT_TIMEOUT)
        assert p.status == 0, "%s installation failed:\n%s" % (project, p.out)

        # Build & Install
        p = Run(['gprbuild', '--relocate-build-tree', '-p', '-P',
                 os.path.join(GSH_ROOT_DIR, 'posix_shell.gpr')] +
                gprbuild_opts,
                cwd=obj_dir, timeout=DEFAULT_TIMEOUT,
                output=None)

        assert p.status == 0, "mains installation failed:\n%s" % p.out

        p = Run(['gprinstall', '-p', '-f', '--prefix=%s' % install_dir,
                 '--relocate-build-tree',
                 '-P', os.path.join(GSH_ROOT_DIR, 'posix_shell.gpr')],
                cwd=obj_dir, timeout=DEFAULT_TIMEOUT)
        assert p.status == 0, "mains installation failed:\n%s" % p.out

    return (os.path.join(install_dir, 'share', 'gpr'),
            os.path.join(install_dir, 'include'),
            build_dir)


def gprbuild(driver,
             project_file=None,
             cwd=None,
             gcov=False,
             scenario=None,
             gpr_project_path=None,
             timeout=DEFAULT_TIMEOUT,
             **kwargs):
    """Launch gprbuild.

    :param project_file: project file to compile. If None, we looks first for
        a test.gpr in the test dir and otherwise fallback on the common
        test.gpr project of the support subdir of the testsuite.
    :type project_file: str
    :param cwd: directory in which to run gprbuild. If None the gprbuild build
        is run in the default working dir for the test.
    :type cwd: str | None
    :param gcov: if True link with gcov libraries
    :type gcov: bool
    :param scenario: scenario variable values
    :type scenario: dict
    :param gpr_project_path: if not None prepent this value to GPR_PROJECT_PATH
    :type gpr_project_path: None | str
    :param kwargs: additional keyword arguements are passed to
        e3.testsuite.process.check_call function
    :return: True on successful completion
    :rtype: bool
    """
    if scenario is None:
        scenario = {}

    if project_file is None:
        project_file = os.path.join(driver.test_env['test_dir'],
                                    'test.gpr')
        if not os.path.isfile(project_file):
            project_file = os.path.join(TESTSUITE_ROOT_DIR,
                                        'support', 'test.gpr')
            scenario['TEST_SOURCES'] = driver.test_env['test_dir']

    if cwd is None:
        cwd = driver.test_env['working_dir']
    mkdir(cwd)
    gprbuild_cmd = [
        'gprbuild', '--relocate-build-tree', '-p', '-P', project_file]
    for k, v in scenario.iteritems():
        gprbuild_cmd.append('-X%s=%s' % (k, v))
    if gcov:
        gprbuild_cmd += ['-largs', '-lgcov', '-cargs',
                         '-fprofile-arcs', '-ftest-coverage', '-g']

    # Adjust process environment
    env = None
    if gpr_project_path:
        new_gpr_path = gpr_project_path
        if 'GPR_PROJECT_PATH' in os.environ:
            new_gpr_path += os.path.pathsep + os.environ['GPR_PROJECT_PATH']
        env = {'GPR_PROJECT_PATH': new_gpr_path}

    check_call(
        driver,
        gprbuild_cmd,
        cwd=cwd,
        env=env,
        ignore_environ=False,
        timeout=timeout,
        **kwargs)
    # If we get there it means the build succeeded.
    return True


class GNATcollTestDriver(TestDriver):
    """Abstract class to share some common facilities."""

    DEFAULT_TIMEOUT = 5 * 60  # 5 minutes

    def should_skip(self):
        """Handle of 'skip' in test.yaml.

        :return: None if the test should not be skipped, a TestStatus
            otherwise.
        :rtype: None | TestStatus
        """
        if 'skip' in self.test_env:
            eval_env = {
                'env': self.env,
                'test_env': self.test_env,
                'disk_space': lambda: df(self.env.working_dir)}

            for status, expr in self.test_env['skip']:
                try:
                    if eval(expr, eval_env):
                        return TestStatus[status]
                except Exception:
                    logging.error(traceback.format_exc())
                    return TestStatus.ERROR
        return None

    @property
    def process_timeout(self):
        """Timeout (in seconds) for subprocess to launch."""
        return self.test_env.get('timeout', self.DEFAULT_TIMEOUT)
