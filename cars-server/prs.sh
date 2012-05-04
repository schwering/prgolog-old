OLD_LPS_IDS=$(pidof lps)
OLD_LPS_PATTERN="$(echo $OLD_LPS_IDS | sed -e 's/ /\\|/g')"

if [ "$OLD_LPS_IDS" != "" ]
then
        echo "Warning: lps already running (process(es) $OLD_LPS_IDS)." >&2
fi


(../lp-server/lps >/dev/null || exit) &

./prs

# Wait for exit and then kill lps:

NEW_LPS_ID=$(pidof lps | sed -e "s/$OLD_LPS_PATTERN//g")
echo "Killing lps instance $NEW_LPS_ID."
kill $NEW_LPS_ID

