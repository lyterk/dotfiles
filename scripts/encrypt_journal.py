import gpg
import os
import tarfile

from gi.repository import GLib
from pydbus import SessionBus


mutex = GLib.Mutex()
home = os.path.expanduser("~")


def encrypt_tarzip():
    with tarfile.open(os.path.join(home, "Documents/org.tgz"), "w:gz") as tar:
        tar.add(
            os.path.join(home, "org"),
            arcname=os.path.basename(os.path.join(home, "org")),
        )
    with open(os.path.join(home, "Documents/org.tgz"), "r") as tar, open(
        "Documents/org.tgz.gpg", "w"
    ) as out:
        encrypted = gpg.encrypt_file(tar, ["kllyter@amazon.com"])
        out.write(encrypted)


def fired_callback(sender, ob, iface, signal, params):
    if not mutex.trylock():
        # This is the second fire. We can exit, because the first fire will
        # handle our needs.
        return

    mutex.unlock()
