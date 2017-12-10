import os

c.colors.completion.category.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.completion.category.border.bottom = os.environ.get('THEME_DARK_GRAY')
c.colors.completion.category.border.top = os.environ.get('THEME_DARK_GRAY')
c.colors.completion.category.fg = os.environ.get('THEME_WHITE')
c.colors.completion.even.bg = os.environ.get('THEME_BLACK')
c.colors.completion.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.completion.item.selected.bg = os.environ.get('THEME_LIGHT_MAGENTA')
c.colors.completion.item.selected.border.bottom = os.environ.get('THEME_LIGHT_MAGENTA')
c.colors.completion.item.selected.border.top = os.environ.get('THEME_LIGHT_MAGENTA')
c.colors.completion.item.selected.fg = os.environ.get('THEME_WHITE')
c.colors.completion.match.fg = os.environ.get('THEME_LIGHT_GRAY')
c.colors.completion.odd.bg = os.environ.get('THEME_BLACK')
c.colors.completion.scrollbar.bg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.completion.scrollbar.fg = os.environ.get('THEME_LIGHT_GRAY')

c.colors.downloads.bar.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.downloads.error.bg = os.environ.get('THEME_RED')
c.colors.downloads.error.fg = os.environ.get('THEME_WHITE')
c.colors.downloads.start.fg = os.environ.get('THEME_WHITE')
c.colors.downloads.stop.fg = os.environ.get('THEME_WHITE')
c.colors.keyhint.fg = os.environ.get('THEME_WHITE')
c.colors.keyhint.suffix.fg = os.environ.get('THEME_YELLOW')
c.colors.messages.error.bg = os.environ.get('THEME_RED')
c.colors.messages.error.border = os.environ.get('THEME_RED')
c.colors.messages.error.fg = os.environ.get('THEME_WHITE')
c.colors.messages.info.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.messages.info.border = os.environ.get('THEME_DARK_GRAY')
c.colors.messages.info.fg = os.environ.get('THEME_WHITE')
c.colors.messages.warning.bg = os.environ.get('THEME_LIGHT_RED')
c.colors.messages.warning.border = os.environ.get('THEME_LIGHT_RED')
c.colors.messages.warning.fg = os.environ.get('THEME_WHITE')
c.colors.prompts.bg = os.environ.get('THEME_BLACK')
c.colors.prompts.border = '1px solid ' + os.environ.get('THEME_WHITE')
c.colors.prompts.fg = os.environ.get('THEME_WHITE')
c.colors.prompts.selected.bg = os.environ.get('THEME_LIGHT_GREEN')
c.colors.statusbar.caret.bg = os.environ.get('THEME_BLUE')
c.colors.statusbar.caret.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.caret.selection.bg = os.environ.get('THEME_LIGHT_MAGENTA')
c.colors.statusbar.caret.selection.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.command.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.statusbar.command.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.command.private.bg = os.environ.get('THEME_LIGHT_GREEN')
c.colors.statusbar.command.private.fg = os.environ.get('THEME_WHITE')
c.colors.statusbar.insert.bg = os.environ.get('THEME_BLACK')
c.colors.statusbar.insert.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.normal.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.statusbar.normal.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.private.bg = os.environ.get('THEME_LIGHT_GREEN')
c.colors.statusbar.private.fg = os.environ.get('THEME_WHITE')
c.colors.statusbar.progress.bg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.url.error.fg = os.environ.get('THEME_RED')
c.colors.statusbar.url.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.url.hover.fg = os.environ.get('THEME_LIGHT_GRAY')
c.colors.statusbar.url.success.http.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.url.success.https.fg = os.environ.get('THEME_LIGHT_CYAN')
c.colors.statusbar.url.warn.fg = os.environ.get('THEME_YELLOW')

c.colors.tabs.indicator.error = os.environ.get('THEME_RED')
c.colors.tabs.indicator.start = os.environ.get('THEME_LIGHT_MAGENTA')
c.colors.tabs.indicator.stop = os.environ.get('THEME_BLUE')

c.colors.tabs.even.bg = os.environ.get('THEME_BLACK')
c.colors.tabs.odd.bg = os.environ.get('THEME_BLACK')
c.colors.tabs.even.fg = os.environ.get('THEME_LIGHT_BLUE')
c.colors.tabs.odd.fg = os.environ.get('THEME_LIGHT_BLUE')

c.colors.tabs.selected.even.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.tabs.selected.odd.bg = os.environ.get('THEME_DARK_GRAY')
c.colors.tabs.selected.even.fg = os.environ.get('THEME_LIGHT_GRAY')
c.colors.tabs.selected.odd.fg = os.environ.get('THEME_LIGHT_GRAY')

c.tabs.background = True

c.completion.height = '40%'
c.completion.web_history_max_items = 0

c.fonts.completion.category = 'bold 11pt monospace'
c.fonts.completion.entry = '11pt monospace'
c.fonts.debug_console = '11pt monospace'
c.fonts.downloads = '11pt monospace'
c.fonts.keyhint = '11pt monospace'
c.fonts.messages.error = '11pt monospace'
c.fonts.messages.info = '11pt monospace'
c.fonts.messages.warning = '11pt monospace'
c.fonts.monospace = 'Iosevka'
c.fonts.prompts = '11pt sans-serif'
c.fonts.statusbar = '11pt monospace'
c.fonts.tabs = '11pt monospace'
c.fonts.web.family.fixed = 'Iosevka'
c.fonts.web.family.sans_serif = 'Noto Sans'
c.fonts.web.family.serif = 'Georgia'
c.fonts.web.family.standard = 'Noto Sans'

c.fonts.hints = '12pt monospace'
c.hints.border = '1px solid ' + os.environ.get('THEME_DARK_GRAY')
c.colors.hints.bg = os.environ.get('THEME_BLACK')
c.colors.hints.fg = os.environ.get('THEME_LIGHT_BLUE')
c.colors.hints.match.fg = os.environ.get('THEME_BLUE')

c.url.searchengines = {
    'DEFAULT': 'https://duckduckgo.com/?q={}',
    'g': 'https://encrypted.google.com/search?q={}',
}

c.url.start_pages = [
    'https://start.duckduckgo.com',
    'https://encrypted.google.com',
    'https://en.wikipedia.org',
]
