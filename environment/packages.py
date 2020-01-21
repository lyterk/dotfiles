#!/usr/bin/env python3

from dataclasses import dataclass
from pathlib import Path


@dataclass
class Package:
    url: str
    installation_location: Path
    is_sudo_required: bool = False

    def is_installed(self) -> bool:
        pass

    def install(self):
        pass

    def uninstall(self):
        pass

    def update(self):
        pass
