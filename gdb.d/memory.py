class memory_window:
    def __init__(self,tui_window):
        self._tui_window = tui_window
        self._tui_window.title = 'Watched Memory'
        self._lines=[]
    def render(self):
        height = self._tui_window.height
        width = self._tui_window.width
        lines = self._lines[-height:]
        self._tui_window.erase()

    def _before_prompt(self):
        self.render()
    def close(self):
        gdb.events.before_prompt.disconnect(self._before_prompt_listener)
gdb.register_window_type('memory',memory_window)

