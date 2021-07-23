# install.packages("webshot")
# install_phantomjs()

# file paths
from_path = '_sessions/'
to_path = '_sessions/_pdf/'

# htmls to be saved as pdf
htmls = c(
  'Welcome/Welcome.html',
  'WhatIsML/WhatIsML.html',
  'RforML/RforML.html',
  'Fitting/Fitting.html',
  'Prediction/Prediction.html',
  'Optimization/Optimization.html',
  'Features/Features.html',
  'Models/Models.html',
  'LookAhead/LookAhead.html'
  )

# get pdf names
pdfs = stringr::str_replace_all(
  sapply(stringr::str_split(htmls, '/'),`[`,2),
  '.html',
  '.pdf')


# save as pdf
for(i in 1:length(htmls)){
  
  webshot::webshot(paste0(from_path, htmls[i]),
                   paste0(to_path, pdfs[i]),
                   vheight = 900 * .8,
                   vwidth = 1600 * .8)
}

# zip files
zip(paste0(to_path, 'MLwR2019Oct_pdfs.zip'),
    c(paste0(to_path, pdfs), paste0(to_path, 'README.rtf'))
    )

