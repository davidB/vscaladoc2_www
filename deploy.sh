git push origin master
git checkout prod
git rebase -i master
mvn clean install stax:deploy -Pstax -Dmaven.test.skip=true
git checkout master