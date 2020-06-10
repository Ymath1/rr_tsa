app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(fileinput = "demo.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
