import os

# Theme
font_size = int(os.environ.get('THEME_FONT_SIZE'))
font_family = os.environ.get('THEME_FONT_FAMILY')
black = os.environ.get('THEME_BLACK')
red = os.environ.get('THEME_RED')
green = os.environ.get('THEME_GREEN')
yellow = os.environ.get('THEME_YELLOW')
blue = os.environ.get('THEME_BLUE')
magenta = os.environ.get('THEME_MAGENTA')
cyan = os.environ.get('THEME_CYAN')
light_gray = os.environ.get('THEME_LIGHT_GRAY')
dark_gray = os.environ.get('THEME_DARK_GRAY')
light_red = os.environ.get('THEME_LIGHT_RED')
light_green = os.environ.get('THEME_LIGHT_GREEN')
light_yellow = os.environ.get('THEME_LIGHT_YELLOW')
light_blue = os.environ.get('THEME_LIGHT_BLUE')
light_magenta = os.environ.get('THEME_LIGHT_MAGENTA')
light_cyan = os.environ.get('THEME_LIGHT_CYAN')
white = os.environ.get('THEME_WHITE')
background = os.environ.get('THEME_BACKGROUND')
foreground = os.environ.get('THEME_FOREGROUND')

c.colors.completion.category.bg = dark_gray
c.colors.completion.category.border.bottom = dark_gray
c.colors.completion.category.border.top = dark_gray
c.colors.completion.category.fg = white
c.colors.completion.even.bg = black
c.colors.completion.fg = light_cyan
c.colors.completion.item.selected.bg = light_magenta
c.colors.completion.item.selected.border.bottom = light_magenta
c.colors.completion.item.selected.border.top = light_magenta
c.colors.completion.item.selected.fg = white
c.colors.completion.match.fg = light_gray
c.colors.completion.odd.bg = black
c.colors.completion.scrollbar.bg = light_cyan
c.colors.completion.scrollbar.fg = light_gray

c.colors.downloads.bar.bg = dark_gray
c.colors.downloads.error.bg = red
c.colors.downloads.error.fg = white
c.colors.downloads.start.fg = white
c.colors.downloads.stop.fg = white
c.colors.keyhint.fg = white
c.colors.keyhint.suffix.fg = yellow
c.colors.messages.error.bg = red
c.colors.messages.error.border = red
c.colors.messages.error.fg = white
c.colors.messages.info.bg = dark_gray
c.colors.messages.info.border = dark_gray
c.colors.messages.info.fg = white
c.colors.messages.warning.bg = light_red
c.colors.messages.warning.border = light_red
c.colors.messages.warning.fg = white

c.colors.prompts.bg = black
c.colors.prompts.border = f'1px solid {white}'
c.colors.prompts.fg = white
c.colors.prompts.selected.bg = light_green

c.colors.statusbar.caret.bg = blue
c.colors.statusbar.caret.fg = light_cyan
c.colors.statusbar.caret.selection.bg = light_magenta
c.colors.statusbar.caret.selection.fg = light_cyan
c.colors.statusbar.command.bg = black
c.colors.statusbar.command.fg = light_blue
c.colors.statusbar.command.private.bg = light_green
c.colors.statusbar.command.private.fg = white
c.colors.statusbar.insert.bg = black
c.colors.statusbar.insert.fg = light_cyan
c.colors.statusbar.normal.bg = black
c.colors.statusbar.normal.fg = light_blue
c.colors.statusbar.private.bg = light_green
c.colors.statusbar.private.fg = white
c.colors.statusbar.progress.bg = light_cyan
c.colors.statusbar.url.error.fg = red
c.colors.statusbar.url.fg = light_cyan
c.colors.statusbar.url.hover.fg = light_gray
c.colors.statusbar.url.success.http.fg = light_cyan
c.colors.statusbar.url.success.https.fg = light_cyan
c.colors.statusbar.url.warn.fg = yellow

c.colors.tabs.indicator.error = red
c.colors.tabs.indicator.start = light_magenta
c.colors.tabs.indicator.stop = blue

c.colors.tabs.even.bg = dark_gray
c.colors.tabs.odd.bg = dark_gray
c.colors.tabs.even.fg = light_green
c.colors.tabs.odd.fg = light_green

c.colors.tabs.selected.even.bg = black
c.colors.tabs.selected.odd.bg = black
c.colors.tabs.selected.even.fg = light_blue
c.colors.tabs.selected.odd.fg = light_blue

c.tabs.background = True

c.completion.height = '40%'
c.completion.web_history_max_items = 0

monospace = '{}pt monospace'.format(font_size)
c.fonts.completion.category = 'bold ' + monospace
c.fonts.completion.entry = monospace
c.fonts.debug_console = monospace
c.fonts.downloads = monospace
c.fonts.keyhint = monospace
c.fonts.messages.error = monospace
c.fonts.messages.info = monospace
c.fonts.messages.warning = monospace
c.fonts.monospace = 'Iosevka'
c.fonts.prompts = str(font_size) + ' sans-serif'
c.fonts.statusbar = monospace
c.fonts.tabs = monospace
c.fonts.web.family.fixed = 'Iosevka'
c.fonts.web.family.sans_serif = 'Noto Sans'
c.fonts.web.family.serif = 'Georgia'
c.fonts.web.family.standard = 'Noto Sans'

c.fonts.hints = '{}pt monospace'.format(font_size - 1)
c.hints.border = '1px solid ' + dark_gray
c.colors.hints.bg = black
c.colors.hints.fg = light_gray
c.colors.hints.match.fg = blue

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'g': 'https://encrypted.google.com/search?q={}',
}

c.url.start_pages = [
    'about:blank',
]

config.bind('a', 'hint all')
config.bind('s', 'hint all tab')
config.bind('b', 'back')
config.bind('w', 'forward')
config.bind('t', 'set-cmd-text -s :open -t')
