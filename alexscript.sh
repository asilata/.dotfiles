while read line;
do
    arg1=`echo $line | awk '{ print $1}'`
    arg2=`echo $line | awk '{ print $2}'`
    arg3=`echo $line | awk '{ print $3}'`
    echo $arg1 $arg2 $arg3 > t_${arg1}_${arg2}_${arg3}_out.txt
done
