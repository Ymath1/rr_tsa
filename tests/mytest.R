app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(fileinput = "demo.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(sidebarItemExpanded = "Overview")
app$setInputs(title = "asd")
app$setInputs(tabs = "Overview_menu")
app$snapshot()
