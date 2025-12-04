#!/usr/bin/env python3
import subprocess
import os

def get_macos_theme():
    try:
        result = subprocess.run(
            ['defaults', 'read', '-g', 'AppleInterfaceStyle'],
            capture_output=True,
            text=True
        )
        return 'dark' if 'Dark' in result.stdout else 'light'
    except:
        return 'light'

def main():
    theme = get_macos_theme()
    config_dir = os.path.expanduser('~/.config/kitty')
    
    if theme == 'dark':
        theme_file = f'{config_dir}/solarized-dark.conf'
    else:
        theme_file = f'{config_dir}/solarized-light.conf'
    
    current_theme = f'{config_dir}/current-theme.conf'
    
    # Create symlink or copy
    if os.path.exists(current_theme):
        os.remove(current_theme)
    os.symlink(theme_file, current_theme)
    
    # Reload kitty
    subprocess.run(['killall', '-SIGUSR1', 'kitty'], stderr=subprocess.DEVNULL)

if __name__ == '__main__':
    main()
