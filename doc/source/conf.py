# -*- coding: utf-8 -*-

extensions = []

templates_path = ['_templates']

source_suffix = '.rst'

master_doc = 'index'

project = u'GSH'
copyright = u'2014, AdaCore'

version = '1.0.0'
release = '1.0.0'

exclude_patterns = []

pygments_style = 'sphinx'

html_theme = 'alabaster'

htmlhelp_basename = 'GSHdoc'


latex_elements = {}

latex_documents = [('index', 'GSH.tex', u'GSH Documentation',
                    u'AdaCorre', 'manual')]

man_pages = [('index', 'gsh', u'GSH Documentation',
              [u'AdaCorre'], 1)]

texinfo_documents = [('index', 'GSH', u'GSH Documentation',
                      u'AdaCorre', 'GSH',
                      'One line description of project.',
                      'Miscellaneous')]
