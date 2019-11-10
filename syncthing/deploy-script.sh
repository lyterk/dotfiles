#!/bin/sh

set -e

for domain in $RENEWED_DOMAINS; do
        case $domain in
        lyterk.com)
                daemon_cert_root=/home/syncthing/.config/syncthing/

                # Make sure the certificate and private key files are
                # never world readable, even just for an instant while
                # we're copying them into daemon_cert_root.
                umask 077

                cp "$RENEWED_LINEAGE/fullchain.pem" "$daemon_cert_root/https-cert.pem"
                cp "$RENEWED_LINEAGE/privkey.pem" "$daemon_cert_root/https-key.pem"

                # Apply the proper file ownership and permissions for
                # the daemon to read its certificate and key.
                chown some-daemon "$daemon_cert_root/https-cert.pem" \
                        "$daemon_cert_root/https-key.pem"
                chmod 400 "$daemon_cert_root/https-cert.pem" \
                        "$daemon_cert_root/https-key.pem"

                service syncthing restart >/dev/null
                ;;
        esac
done
