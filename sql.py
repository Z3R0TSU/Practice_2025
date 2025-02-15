import mysql.connector

db = mysql.connector.connect(
    host = "localhost",
    user = "root",
    passwd="1234"
)
 
mycursor = db.cursor()
