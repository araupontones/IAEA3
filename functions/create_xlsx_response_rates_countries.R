#Create responses by country in excel

crate_xlsx_by_country <- function(db,exfile){
  
  
  rows = nrow(db) + 1
  wb <- openxlsx::createWorkbook()
  addWorksheet(wb, "Countries")
  writeData(wb, "Countries", db, rowNames = F, colNames = T)
  
  #styles
  style_header <- createStyle(fontName = 'Open Sans',
                              fontSize = 12,
                              fontColour = '#FFFFFF',
                              textDecoration = 'bold',
                              bgFill = 'black',
                              border = "TopBottomLeftRight",
                              borderColour = '#FFFFFF')
  
  addStyle(wb, "Countries", style = style_header, rows = 1, cols = 1:7)
  
  style_center <- createStyle(halign = "center",
                              fontName = 'Open Sans',
                              fontSize = 11,
                              fontColour = 'black',
                              border = "TopBottomLeftRight",
                              borderColour = 'black')
  
  addStyle(wb, "Countries", style = style_center, rows = 2:rows, cols = 2:7, gridExpand = T)
  
  style_countries= createStyle(fontName = 'Open Sans',
                               fontSize = 11,
                               fontColour = 'black',
                               border = "TopBottomLeftRight",
                               borderColour = 'black')
  
  
  addStyle(wb, "Countries", style = style_countries, rows = 2:rows, cols = 1, gridExpand = T)
  
  
  style_totals = createStyle(fontName = 'Open Sans',
                             fontSize = 12,
                             fontColour = '#FFFFFF',
                             textDecoration = 'bold',
                             bgFill = 'black',
                             border = "TopBottomLeftRight",
                             borderColour = '#FFFFFF')
  
  
  addStyle(wb, "Countries", style = style_totals, rows= rows, cols = 1:7)
  
  style_totals_center = createStyle(fontName = 'Open Sans',
                                    halign = 'center',
                                    fontSize = 12,
                                    fontColour = '#FFFFFF',
                                    textDecoration = 'bold',
                                    bgFill = 'black',
                                    border = "TopBottomLeftRight",
                                    borderColour = '#FFFFFF')
  
  
  addStyle(wb, "Countries", style = style_totals_center, rows= rows, cols = 2:7)
  
  #conditional formatting
  neg_style= createStyle(fontName = 'Open Sans',
                               fontSize = 11,
                               fontColour = 'white',
                               border = "TopBottomLeftRight",
                               borderColour = 'black',
                               bgFill = 'red')
  
  conditionalFormatting(wb, "Countries",
                        cols = 1,
                        rows = 2:rows,
                        rule = "$E2==0",
                        style = neg_style
                        )
  
  
  
  setColWidths(wb, 'Countries', cols = 1, widths = 29)
  setColWidths(wb, 'Countries', cols = c(2,3,5), widths = 13)
  setColWidths(wb, 'Countries', cols = 6, widths = 33)
  setColWidths(wb, 'Countries', cols = 7, widths = 18)
  
  openxlsx::saveWorkbook(wb, exfile, overwrite = TRUE)
  
  cli::cli_alert_success("Report saved!")
  
}
