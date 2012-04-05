F=$1

#for f in $(ls obs)
#do
#        if [ -f "$f" ]
#        then
                rm -rf "traces/" "obs/traces-$F" || exit
                mkdir "traces/" || exit
                cat "obs/$F" | ./cars || exit
                ./gen_traces.sh || true
                mv "traces" "obs/traces-$F" || exit
#        fi
#done

