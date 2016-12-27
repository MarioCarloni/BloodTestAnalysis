# BloodTestAnalysis
Return wide dataset given Comprehensive Metabolic and Lipid Panels

```
drsOrders <- function(anionGap,hco3,glucose,albgluRatio,hdl,ldl, trig) {
  
  lipid = lipidPanel(hdl,ldl, trig)
  
  compmeta = compMeta(anionGap,hco3,glucose,albgluRatio)
  
  data = data.frame(lipid, compmeta)
  
  data
}

# Change values here
example <- drsOrders(13,27,90,2.1,39,213,166)

print(example)
```

Dataset should look like this:
![](http://i.imgur.com/YzU4t8A.png)
