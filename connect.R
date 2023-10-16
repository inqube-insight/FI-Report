library(mongolite)

mongourl <- paste("mongodb://MongodbAdmin:t%262uO%7Cr_8%5D4%5DJ68@sg-int-documentdb-aws-mongodb-prod.cluster-c2zqprjq3kw1.ap-southeast-1.docdb.amazonaws.com:27017/?ssl=true&",
                  
                  "readPreference=secondaryPreferred&replicaSet=rs0", sep="")

rm.data.new =mongo('Recon_Data',db='Weekly_Reconcilation_Report',url=mongourl,options = ssl_options(weak_cert_validation = TRUE))

#Create a MongoDB client, open a connection to Amazon DocumentDB as a replica

#   set and specify the read preference as secondary preferred

client <- mongo(url = mongourl, options = ssl_options(weak_cert_validation = TRUE, ca ="global-bundle.pem"))



#Insert a single document

str <- c('{"hello" : "Amazon DocumentDB"}')

rm.data.new$insert(str)



#Find the document that was previously written

rm.data.new$find()
