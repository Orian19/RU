mkdir mydir
cp -r /share/ex_data/ex1/lyrics/* ./mydir
cd mydir
mv ./old_drafts ./archive
cp final_draft ./archive/draft4.txt
rm archive/*[13]*
ls -a ./archive/  > files
echo "==== END OF FILE ====" >> ./files
chmod -R a+w ./archive
chmod 651 final_draft
