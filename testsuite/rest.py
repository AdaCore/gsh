"""Implement some utility function that ease generation of ReST code"""

HEADING_CHARS = ['#', '*', '=', '-', '^', '"']


def heading(title, heading_char):
    if isinstance(heading_char, int):
        heading_char = HEADING_CHARS[heading_char]

    result = "\n\n"

    # Only parts and chapters are overlined
    if heading_char == '#' or heading_char == '*':
        result += heading_char * len(title) + '\n'

    result += title + '\n'
    result += heading_char * len(title) + '\n\n'
    return result


def part(title):
    return heading(title, HEADING_CHARS[0])


def chapter(title):
    return heading(title, HEADING_CHARS[1])


def section(title):
    return heading(title, HEADING_CHARS[2])


def subsection(title):
    return heading(title, HEADING_CHARS[3])


def subsubsection(title):
    return heading(title, HEADING_CHARS[4])

def paragraph(title):
    return heading(title, HEADING_CHARS[5])


def toctree(item_list, depth=2, hidden=False):
    result = "\n"
    result += ".. toctree::\n"
    if hidden:
        result += "   :hidden:\n"
    result += "   :maxdepth: %s\n\n" % depth
    
    for item in item_list:
        result += "   %s\n" % item
    result += "\n"
    return result


def emphasis(content):
    return "*" + content + "*"


def strong(content):
    return "**" + content + "**"


def generic_block(command, content, command_arg=None):
    if content == '':
        return ''

    if command_arg is None:
        command_arg = ''

    result = "\n.. %s:: %s\n\n" % (command, command_arg)
    result += '\n'.join(['   ' + line for line in content.splitlines()])
    result += '\n\n'
    return result


def warning(content):
    return generic_block('warning', content)


def parsed_literal(content):
    return generic_block('parsed-literal', content)


def code_block(content, language):
    return generic_block('code-block', content, language)


def raw(content, doc_type):
    return generic_block('raw', content, doc_type)

def underline_bold(content):
    return raw('% Use bold and underline\n\n' +
               '\\underline{\\textbf{' + content + '}}', "latex")

def underline_bold_center(content):
    return raw('% Use bold, underline and center\n\n' +
               '\\begin{center}\n\underline{\\textbf{' + content + '}}\n' + \
               '\\end{center}', "latex")

def line_block(content):
    result = '\n\n'
    result += '\n'.join(['   ' + line for line in content.splitlines()])
    return result

def item_block(content, symbol='*', spacing='  ', indentation=''):
    result = ''
    for line in content.splitlines():
       if result == '':
          # First line needs the itemize symbol
          result = indentation + symbol + spacing[1:] + line + '\n'
       else:
          result += indentation + spacing + line + '\n'
    return result

def label(content):
    result = '\n\n'
    result += '.. _%s:\n\n' % content
    return result
