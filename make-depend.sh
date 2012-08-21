DIRS="$@"
DEPEND_FILE=".mercury-depend"

for DIR in $DIRS
do
        FILES="$(find $DIR -maxdepth 1 -name \*.m | xargs cat | grep '^:- \(use\|import\)_module' | sed -e 's/^:- \(use\|import\)_module //g' | sed -e 's/\.$/\.m /g')"
        #FILES=" $(find $DIR -maxdepth 1 -name \*.h -or -name \*.c -or -name \*.cc -or -name \*.m | xargs cat | grep '#include' | sed -e 's/^\(.\+\|\)#include//g' | sed -e 's/\("\|<\)/ /' | sed -e 's/\("\|>\)\(.\+\|\)$/ /g' | sed -e 's/^.\+\///g')"
        echo "# $DIR" >$DIR/$DEPEND_FILE
        echo "mercury-depends:" >>$DIR/$DEPEND_FILE
        for FILE in $FILES
        do
                DEP=$(find $DIRS -name $FILE | sed -e 's/\/.\+$//g')
                if [ "$DEP" != "" -a "$DEP" != "$DIR" -a "$(grep "$DEP" $DIR/$DEPEND_FILE)" == "" ]
                then
                        echo "$DIR -> $DEP"
                        echo "	@make -C ../$DEP" >>$DIR/$DEPEND_FILE
                        if [ -f "$DEP/$DEPEND_FILE" -a "$(grep $DIR "$DEP/$DEPEND_FILE" )" != "" ]
                        then
                                echo "WARNING: direct circular dependency $DEP <-> $DIR" >&2
                        fi
                fi
        done
done

