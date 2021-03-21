#!/usr/bin/expect
set FILE1 [lindex $argv 0];
set FILE2 [lindex $argv 1];
set FILE3 [lindex $argv 2];
set FILE4 [lindex $argv 3];
spawn java -jar /opt/wings/storage/default/users/admin/genomics/code/library/extractVariantPeptides/variant_peptide.jar # actual command here
expect "input//path//to//peptide_table: "
send "$FILE1\r"
expect "input//path//to//customized_db(e.g w2_customized.fasta): "
send "$FILE2\r"
expect "input//path//to//normal_refseq_db(e.g refseq_pro.fasta): "
send "$FILE3\r"
expect "input//path//to//ouput//filefolder//: "
send "$FILE4\r"
expect eof
