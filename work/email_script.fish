#!/usr/bin/fish

set nm /usr/bin/notmuch
set me "kllyter"
set boss "mansataa" "tompcase" "saindane"
set director "joannste"
set team "weny" "nicwrin" "zhaoyic" "amyni" "varuojha" "adheep" "yjiaa"
set domain "@amazon.com"
set retrieval "Retrieval using the IMAP4 protocol failed for the following message"
set deinbox -inbox -unread

eval $nm new

function nm_tag
    /usr/bin/notmuch tag $argv
end

# More likely to be to me

nm_tag +to_me "to:$me$domain AND date:today"

nm_tag +boss_to_me +boss "to:$me$domain AND from:$boss$domain"

nm_tag +boss_broadcast +boss "to:skinet$domain AND from:$boss$domain"

nm_tag +joannste_team "to:joanste-team$domain"
nm_tag +director "from:$director$domain"

for user in $team
    nm_tag +team "from:$user$domain OR to:$user$domain"
end

# More likely to be automated

nm_tag +automated_cruft +trash +deleted -inbox -unread "Cron AND from:root@"

nm_tag +automated_cruft +trash +deleted -inbox -unread "$retrieval"

nm_tag +dwp +trash -inbox -unread "from:dwp"

nm_tag +tickets "from:remedy$domain OR (Ticket AND Correspondence)"

nm_tag +permissions "from:permissions-notifier"

nm_tag +sim from:"issues@prod.sim.a2z.com"

# Interest lists and chatter

set interest_lists "bdt-interest" "djangohackers" "dryad-interest" "emacs" "emr-interest" "etl-users" "hammerstone-interest" "motorbikes" "pythonhackers" "quicksight-interest" "redshift-interest" "apache-spark-interest" "ubuntu-users" "aaa-interest" "coe-interest"

for list in $interest_lists
    nm_tag -inbox +$list +interest_list "to:$list$domain"
end

for list in $chatter_lists
    nm_tag -inbox -unread +$list "to:$list"
end
