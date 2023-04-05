function create_git
    argparse 'h/help' 'p/package' 'l/location' -- $argv
    or return

    if set -ql _flag_help
        echo "create_git [-h|--help] [-p|--package] Run inside the directory to create a bare repo and make it the remote"
        return 0
    end
    set repo_name "/git/$package.git"

    ssh sudonuc "git init --bare $repo_name; chown -R git:git $repo_name"

    git remote add origin gitserv:$remote_name
end
