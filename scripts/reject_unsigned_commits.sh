#!/usr/bin/env bash
##############################################################################
#
# check-commit-signature
# ----------------------
# A server-side update git hook for checking the GPG signature of a pushed
# commit.
#
# To enable this hook, rename this file to "update".
#
# Original author: Isis Agora Lovecruft, 0x2cdb8b35
# Updated: Nikola Kotur, kotnick@gmail.com
##############################################################################

## committers and their signing GPG key fingerprints should go in the
## ${collaborators} array:
##
## we hard code the fingerprint to protect against the possibility of the gpg
## on the remote git server updating the keys (if it is configured to do so,
## and also configured to use short keyids) and pulling in a colliding key from
## a keyserver.
##
## Get the fingerprint using:
##    gpg --fingerprint KEY_ID | grep -o -E "([0-9A-F]{4}[[:space:]]{0,2}){10}"
declare -A collaborators
collaborators['lyterk']='A81D CCE8 555B 5DBF 52B5  30CF B1E5 BE18 EFA4 DE90'

## get the keyids from the fingerprints above:
declare -A trusted_keys
for collab in "${!collaborators[@]}"; do
    fpr="${collaborators["$collab"]}"
    shortid=$(echo "$fpr" | cut -d ' ' --output-delimiter='' -f 10,11 )
    trusted_keys["$collab"]="$shortid"
done

## $1 is the git ref which is being revised
## $2 is the last HEAD
## $3 is the HEAD commit of the series of commits being applied
ref=$1
rev_old=$2
rev_new=$3
span=$(git rev-list $rev_old..$rev_new)

## check all the commits in the series
rev_cur=$rev_old                   ## set the current rev to the previous HEAD
for commit in $span ; do
    check_rev=$(git rev-parse --verify "$commit")
    hash_len="${#check_rev}"
    if [[ "$hash_len" != 40 ]]; then
        echo "*** Commit hash for commit $commit has bad length of $hash_len " >&2
        exit 1
    fi

    ## get the commit type of the current rev:
    ## a commit with a hash full of zeros is a deletion of a ref
    zero="0000000000000000000000000000000000000000"
    merge=$(git rev-list -n 1 --merges "$rev_cur".."$commit")
    if [ "$commit" = "$zero" ]; then
        commit_type=delete
    else
        if test -n "$merge"; then
            commit_type=merge
        else
            commit_type=$(git cat-file -t "$commit")
        fi
    fi

    ## the following returns non-null if $rev_cur has a signature, and gpg reports
    ## the signature is good:
    has_good_sig=$(git log --format=%H --show-signature "$commit" | \
        grep "Good signature")

    ## the following extracts the signing keyid (either short or long) from the
    ## signature on $rev_cur:
    signing_keyid=$(git show --show-signature "$commit" | \
        grep -o -E "key ID [0-9A-Fa-f]{8,16}" | \
        cut -d ' ' -f 3 )

    fpr_signing_keyid=$(gpg --fingerprint $signing_keyid | \
        grep -o -E "([0-9A-F]{4}[[:space:]]{0,2}){10}")

    ## the following is the short reference name for tags, i.e. 'v0.1.2' in
    ## lieu of 'refs/tags/v0.1.2':
    short_ref=${ref##refs/tags/}

    case "$ref","$commit_type" in
        refs/heads/master,commit)
            allowed_keyid=false
            if test -n "$has_good_sig" -a -n "$signing_keyid" ; then
                for collab in "${!trusted_keys[@]}"; do
                    ## XXX the $keyid is currently only a short key id
                    ## because git doesn't give us a way to ask for a
                    ## long keyid
                    keyid="${trusted_keys["$collab"]}"
                    if [[ "$signing_keyid" == "$keyid" ]]; then
                        ## check that the fingerprint of the key that
                        ## gave us a good signature matches the
                        ## hardcoded ones:
                        if [[ "$fpr_signing_keyid" == "${collaborators["$collab"]}" ]]; then
                            allowed_keyid=true
                            echo "*** accepted: good signature on ${commit:0:8} by $collab with key id $signing_keyid " >&2
                        fi
                    fi
                done
            else
                echo "*** rejected: missing or bad signature on commit ${commit:0:8}" >&2
                exit 1
            fi

            if ! $allowed_keyid ; then
                echo "*** rejected: merges to master must be signed with one of the following:" >&2
                for key in "${!trusted_keys[@]}"; do
                    echo "***     $key" >&2
                done
                exit 1
            fi
            ;;
        *)
            ;;
    esac
    ## increment the current rev to the $commit we just checked:
    rev_cur=$commit
done
