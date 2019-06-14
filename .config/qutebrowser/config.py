from string import ascii_uppercase

#
# Visual
#
c.qt.highdpi = True
c.zoom.default = '100%'

# Status bar
c.tabs.show = 'never'
c.scrolling.bar = 'never'
c.statusbar.widgets = ['keypress', 'url', 'scroll', 'progress']
c.colors.statusbar.normal.bg = '#303030'
c.colors.statusbar.passthrough.bg = '#262626'
c.colors.statusbar.normal.fg = '#839496'
c.colors.statusbar.passthrough.fg = '#839496'
c.colors.statusbar.url.success.http.fg = '#839496'
c.colors.statusbar.url.success.https.fg = '#839496'

# Fonts
c.fonts.completion.entry = '11pt Hack'
c.fonts.completion.category = 'bold 11pt Hack'
c.fonts.debug_console = '11pt Hack'
c.fonts.downloads = '11pt Hack'
c.fonts.hints = 'bold 11pt Hack'
c.fonts.keyhint = '11pt Hack'
c.fonts.messages.error = '11pt Hack'
c.fonts.messages.info = '11pt Hack'
c.fonts.messages.warning = '11pt Hack'
c.fonts.prompts = '11pt sans-serif'
c.fonts.statusbar = '11pt Hack'
c.fonts.tabs = '11pt Hack'


#
# Key bindings/command aliases
#
c.bindings.commands = {
        'passthrough': {
            '<Ctrl-6>': 'tab-focus last',
            '<Ctrl-e>': 'open-editor',
            '<Ctrl-f>': 'scroll right',
            '<Ctrl-b>': 'scroll left',
            '<Ctrl-p>': 'scroll up',
            '<Ctrl-n>': 'scroll down',
            '<Ctrl-u><Ctrl-x><Ctrl-f>': 'set-cmd-text -s :open',
            '<Ctrl-x><Ctrl-c>': 'quit --save',
            '<Ctrl-x><Ctrl-f>': 'set-cmd-text -s :open -t',
            '<Ctrl-x>k': 'tab-close',
            '<Ctrl-x><Ctrl-m>': 'set-cmd-text :',
            '<Ctrl-x><Ctrl-s>': 'session-save',
            '<Ctrl-x>[': 'tab-prev',
            '<Ctrl-x>]': 'tab-next',
            '<Ctrl-x>b': 'set-cmd-text -s :buffer',
            },
        'insert': {
            '<Ctrl-6>': 'tab-focus last',
            '<Ctrl-e>': 'open-editor',
            '<Ctrl-f>': 'scroll right',
            '<Ctrl-b>': 'scroll left',
            '<Ctrl-p>': 'scroll up',
            '<Ctrl-n>': 'scroll down',
            '<Ctrl-u><Ctrl-x><Ctrl-f>': 'set-cmd-text -s :open',
            '<Ctrl-x><Ctrl-c>': 'quit --save',
            '<Ctrl-x><Ctrl-f>': 'set-cmd-text -s :open -t',
            '<Ctrl-x>k': 'tab-close',
            '<Ctrl-x><Ctrl-m>': 'set-cmd-text :',
            '<Ctrl-x><Ctrl-s>': 'session-save',
            '<Ctrl-x>[': 'tab-prev',
            '<Ctrl-x>]': 'tab-next',
            '<Ctrl-x>b': 'set-cmd-text -s :buffer',
            },
        'normal': {
            '<Tab>': 'nop',
            }
        }

c.bindings.commands['command'] = {
	'<ctrl-s>': 'search-next',
	'<ctrl-r>': 'search-prev',

	'<ctrl-p>': 'completion-item-focus prev',
	'<ctrl-n>': 'completion-item-focus next',

	'<alt-p>': 'command-history-prev',
	'<alt-n>': 'command-history-next',

	# escape hatch
	'<ctrl-g>': 'leave-mode',
}

# Aliases 
c.aliases = {
        'q': 'close',
        'qa': 'quit',
        'qall': 'quit',
        'w': 'session-save',
        'wq': 'quit --save'
    }

config.unbind('/', mode='normal')
config.unbind('\'', mode='normal') # unbind jump-to-mark for global jumps
config.unbind('m', mode='normal')
config.unbind('<Ctrl-x>', mode='normal')
config.unbind('<Ctrl-w>', mode='normal')
config.unbind('co', mode='normal')
config.unbind('<J>',      mode='normal')
config.unbind('<K>',      mode='normal')
config.unbind('<Ctrl-u>', mode='normal')
config.unbind('<Ctrl-h>', mode='normal')
config.unbind('J', mode='normal')
config.unbind('K', mode='normal')
config.unbind('d', mode='normal')
config.unbind('o', mode='normal')

for char in ascii_uppercase:
    config.bind(f'm{char}', f'spawn --userscript qute_mark {char}')


config.bind('<Ctrl-6>', 'tab-focus last')
config.bind('<Ctrl-e>', 'open-editor')
config.bind('<Ctrl-s>', 'set-cmd-text /')
config.bind('<Ctrl-f>', 'scroll right')
config.bind('<Ctrl-b>', 'scroll left')
config.bind('<Ctrl-p>', 'scroll up')
config.bind('<Ctrl-n>', 'scroll down')
config.bind('<Ctrl-x>b', 'set-cmd-text -s :buffer')
config.bind('<Ctrl-x>]', 'tab-next')
config.bind('<Ctrl-x>[', 'tab-prev')
config.bind('<Ctrl-x>k', 'tab-close')
config.bind('<Ctrl-x><Ctrl-s>', 'session-save')
config.bind('<Ctrl-x><Ctrl-c>', 'quit --save')
config.bind('<Ctrl-x><Ctrl-m>', 'set-cmd-text :')
config.bind('<Ctrl-x><Ctrl-f>', 'set-cmd-text -s :open -t')
config.bind('<Ctrl-u><Ctrl-x><Ctrl-f>', 'set-cmd-text -s :open')
config.bind('<Ctrl-k>',       'back')
config.bind('<Ctrl-j>',       'forward')
config.bind('k',              'scroll up')
config.bind('j',              'scroll down')
config.bind('<Ctrl-o>',       'set-cmd-text -s :open -t')
config.bind('<Ctrl-shift-o>', 'set-cmd-text -s :open')
config.bind('<Ctrl-p>',       'scroll up')
config.bind('<Ctrl-n>',       'scroll down')
config.bind('tb',             'set-cmd-text -s :tab-take')
config.bind(']b',             'tab-next')
config.bind('[b',             'tab-prev')
config.bind('<Esc>',          'fake-key <Esc>')

#
# Misc settings
#
c.url.start_pages = ['https://www.codesections.com']
c.tabs.select_on_remove = 'last-used'
c.spellcheck.languages = ['en-US']
# Editor (and arguments) to use for the `open-editor` command. The
# following placeholders are defined: * `{file}`: Filename of the file
# to be edited. * `{line}`: Line in which the caret is found in the
# text. * `{column}`: Column in which the caret is found in the text. *
# `{line0}`: Same as `{line}`, but starting from index 0. * `{column0}`:
# Same as `{column}`, but starting from index 0.
# Type: ShellCommand
c.editor.command = ['vi-split', '{file}']

c.window.title_format = 'Qutebrowser'
c.tabs.select_on_remove = 'last-used'
c.tabs.new_position.unrelated = 'next'
c.tabs.new_position.related = 'next'
c.tabs.last_close = 'blank'
c.auto_save.session = True
# c.session.lazy_restore = True # If we save buffers, don't load them all

c.input.insert_mode.auto_enter = False
c.input.insert_mode.auto_leave = False
c.tabs.mode_on_change = 'persist'
c.input.insert_mode.auto_load = False
c.input.insert_mode.leave_on_load = False
c.input.insert_mode.plugins = False

c.input.links_included_in_focus_chain = False
c.input.partial_timeout = 10000 # We have <C-g>

# JavaScript settings.
config.set('content.javascript.enabled', True, 'file://*')
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'qute://*/*')


config.source('nord-qutebrowser.py')
