enc () {
    tar -czf ~/journal.tz ~/org -C ~/org .
    gpg --output ~/journal.tz.gpg --symmetric ~/journal.tz
    retval=$?
    rm -rf ~/org ~/journal.tz
}

dec () {
    gpg --output ~/journal.tz --decrypt ~/journal.tz.gpg
    retval=$?
    test $retval --ne 0 && 
    tar -xvf ~/journal.tz
    rm ~/journal.tz.gpg ~/journal.tz
}
