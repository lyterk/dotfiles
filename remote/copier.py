#!/usr/bin/env python

import os
import errno

home = os.path.expanduser('~')
dotfiles_path = home + '/dotfiles/remote'

files = {
    'config': home + '/.ssh/config',
    'config.fish': home + '/.config/fish/config.fish',
    'gitconfig': home + '/.gitconfig',
    'gitignore': home + '/.gitignore',
    'spacemacs': home + '/.spacemacs',
    'aspell.en.pws': home + '/.aspell.en.pws',
    'pylintrc': home + '/.pylintrc'
}

# Remove existing symlinked destinations
for _, value in files.items():
    try:
        os.unlink(value)
    except OSError as o:
        if o.errno == errno.EEXIST:
            pass
        else:
            raise
    except Exception as exc:
        print(exc)
    try:
        parent = os.path.dirname(value)
        os.makedirs(parent)
    except OSError as o:
        if o.errno == errno.EEXIST:
            pass
        else:
            raise
    except Exception:
        raise


# Create new symlink from dotfiles directory
[os.symlink(os.path.join(dotfiles_path, key), value) for key, value in files.items()]
