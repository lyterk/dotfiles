#!/usr/local/bin/fish

set nm /usr/local/bin/notmuch
set me "kllyter"
set boss "skipark" "flatleyk"
set team "skipark" "hcary" "foot" "djcoop"
set relatives "teaff" "albaum" "flatleyk" "bingrou" "esthekim" "kandiks" "sinduri"
set domain "@amazon.com"
set retrieval "Retrieval using the IMAP4 protocol failed for the following message"
set password_notify "Project Austin Redshift Cluster - Monthly Automated Password Change"
set deinbox -inbox -unread

eval $nm new

function nm_tag
    /usr/local/bin/notmuch tag $argv
end

# More likely to be to me

nm_tag +to_me "to:$me$domain AND date:today"

nm_tag +boss_to_me +boss "to:$me$domain AND from:$boss$domain"

nm_tag +boss_broadcast +boss "to:skinet$domain AND from:$boss$domain"

nm_tag +ball_so_hard "to:ball-so-hard"

for user in $team
    nm_tag +team "from:$user$domain OR to:$user$domain"
end

for user in relatives
    nm_tag +distant_team "from:$user$domain OR to:$user$domain"
end

# More likely to be automated

nm_tag +automated_cruft +trash +deleted -inbox -unread "Cron AND from:root@"

nm_tag +automated_cruft +trash +deleted -inbox -unread "$retrieval"

nm_tag +dwp +trash -inbox -unread "from:dwp"

nm_tag +datanet_monitor -inbox -unread "from:fba-analytics-routine"

nm_tag +airflow_auto -inbox "from:fba-airflow"

nm_tag +worthless_cheng -inbox -unread "from:fbaanalyticsdata"

nm_tag +passwords -inbox $password_notify

nm_tag +alarms -inbox "from:fba-analysis-alarms"

nm_tag +tickets "from:remedy$domain OR (Ticket AND Correspondence)"

nm_tag +permissions "from:permissions-notifier"

nm_tag +trash -inbox -unread "Undeliverable: QA Flash for"

# Interest lists and chatter

set interest_lists "bdt-interest" "djangohackers" "dryad-interest" "emacs" "emr-interest" "etl-users" "hammerstone-interest" "motorbikes" "pythonhackers" "quicksight-interest" "redshift-interest" "apache-spark-interest" "ubuntu-users" "aaa-interest"

set chatter_lists "seattle-chatter"

for list in $interest_lists
    nm_tag -inbox +$list "to:$list"
end

for list in $chatter_lists
    nm_tag -inbox -unread +$list "to:$list"
end
