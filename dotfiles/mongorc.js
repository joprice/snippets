
function dropAllDatabases() {
  var mongo = db.getMongo();
  var dbs = mongo.getDBNames();
  dbs.forEach(function(name) {
    mongo.getDB(name).dropDatabase();
  })
}

