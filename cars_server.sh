# Runs cars-server and lp-server (so that they can communicate).

CARS_SERVER=cars_server
LP_SERVER=lp_server

OLD_LPS_IDS=$(pidof $LP_SERVER)
OLD_LPS_PATTERN="$(echo $OLD_LPS_IDS | sed -e 's/ /\\|/g')"

function clean_up {
        if [ "$OLD_LPS_PATTERN" != "" ]
        then
                NEW_LPS_ID=$(pidof $LP_SERVER | sed -e "s/$OLD_LPS_PATTERN//g")
        else
                NEW_LPS_ID=$(pidof $LP_SERVER)
        fi
        if [ "$NEW_LPS_ID" != "" ]
        then
                echo "Killing $LP_SERVER instance $NEW_LPS_ID"
                kill $NEW_LPS_ID
        fi
}

if [ "$OLD_LPS_IDS" != "" ]
then
        echo "Warning: $LP_SERVER already running (process(es) $OLD_LPS_IDS)" >&2
fi

(lp-server/$LP_SERVER >/dev/null || exit) &

trap clean_up SIGHUP SIGINT SIGTERM
cars-server/$CARS_SERVER
clean_up

