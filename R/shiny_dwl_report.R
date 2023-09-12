#` Shiny module to generate word doc for download
#'
#'
#' Generates WORD reports, for the following LIST of input elements
#'    - doc_titel = character(1)
#'    - sec_title = character(number of sections)
#'    - sec_para = list(para1 =
#'                      para2 = ....
#'                      number of para)
#'    - sec_table = list(para1 = ...)
#'    - sec_figure = list(para1 = ...)
#'
#'
#' @keywords internal
#' @noRd


# UI
dwl_reportUI<-function(id, btnlabel = "Generate Report") {
  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1; margin:0% 20% 0% 0%;")

  styleActButtonActivate<-c("color: #FFFFFF; background-color: #0d47a1;
                  border-color: #0d47a1;  display: block;margin-left: auto;margin-right: auto;")
  styleDwlButton<-c("color: #FFFFFF;  width: 180px;background-color: #1976D2;
                  border-color: #1976D2;
                  margin:0 20% 0 20%;")

  invisibleButton<-c("color: #FFFFFF; background-color: #FFFFFF; visibility: hidden;
                  border-color: #FFFFFF; margin:0% 0% 0% 0%;height:1px;")
  ns <- NS(id)
  #############################
  ## output format is dynamic
  ##    - leafletouput for small
  ##    - mapdeck for large
  tagList(
    shinyjs::useShinyjs(),
    actionButton(ns("generateReportInt"),
                 btnlabel,
                 icon("download"), width = "50%",
                 style=styleActButtonActivate),

    downloadButton(ns("dwl_report"), "inv", style=invisibleButton)

  )
  ####################FIN UI####################################################
}
# SERVER
dwl_reportSRV<-function(id, content = NULL,
                        fn = "QuestionnaireManual",
                        wordstyles = "inst/rmdfiles/FINAL_report_for_download.docx",
                        pptxstyles = NULL,
                        type = c("pptx", "word")) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Initate Report Creation
      observeEvent(input$generateReportInt, {

        rep_cont<-content()
        req(rep_cont)

        # 1. Word Document
        if(type=="word") {
          req(wordstyles)
          #####################
          ## 1. Get Content
          ### 2.1 Generate File
          shiny::withProgress(message = 'Preparing Word Document', value = 0,{
            doc.full<-read_docx(wordstyles) %>%
              set_doc_properties(title = "Survey Solutions Questionnaire Manual",
                                 creator = "Questionnaire Manual Application v1.0.0",
                                 created = Sys.time())




            ## 2.2 DOC TITLE & DATE
            doc.full <- doc.full  %>%
              body_add(rep_cont$doc_title) %>%
              body_add_break()
            ## 2.3. Add Section Para and Tables
            ##    i. loop over section
            ##        ii. loop over para
            incProgress(0.2)
            for (sec_para in names(rep_cont$sec_para)) {
              #incProgress(0.2)
              p<-rep_cont$sec_para[[sec_para]]
              t<-rep_cont$sec_table[[sec_para]]
              i<-rep_cont$sec_graph[[sec_para]]
              sectitle<-rep_cont$sec_title[[sec_para]]
              doc.full <- doc.full  %>%
                body_add_par(sectitle, style = "heading 2") %>%
                body_add_par(NULL, style = "Normal")
              for (para in names(p)) {
                pp<-p[[para]]
                tt<-t[[para]]
                ii<-i[[para]]
                ## add para
                if (!is.null(pp)) {
                  doc.full <- doc.full  %>%
                    body_add(pp, style = "Normal")
                }
                ## add table
                if (!is.null(tt)) {
                  doc.full<-doc.full %>%
                    body_add_table(tt,
                                   style = "Grid Table 6 Colorful Accent 2",
                                   header = T) %>%
                    body_add_par(NULL, style = "Normal") %>%
                    body_add_par(NULL, style = "Normal")%>%
                    body_add_break()

                }
                ## add graph
                if (!is.null(ii)){
                  doc.full<-doc.full %>%
                    body_add_gg(ii, style = "Figure") %>%
                    body_add_par(NULL, style = "Normal") %>%
                    body_add_par(NULL, style = "Normal") %>%
                    body_add_break()
                }

              }
              ## page break after each section
              doc.full<-doc.full %>%
                body_add_break()
            }
            ## 2.4. Tempfile
            ##############################
            ## 2. Switch working directory for report
            withr::with_dir(tempdir(),
                            {doc.full %>%
                                print(target = "report_for_download_v1.docx")}
            )
          })
        } else if(type=="pptx"){
          # 1. PPTX Document
          req(pptxstyles)
          creatorsuso<-"Questionnaire Manual Application v1.0.0"
          shiny::withProgress(message = 'Preparing PowerPoint Presentation', value = 0,{
            doc.full<-read_pptx(pptxstyles) %>%
              set_doc_properties(title = "Survey Solutions Questionnaire Manual",
                                 creator = creatorsuso,
                                 created = Sys.time()) %>%
              officer::remove_slide(index = 1)

            # DEBUG
            #pptxstyles<-c("./inst/rmdfiles/wb_dg_suso_modern.pptx")
            #annotate_base(path = pptxstyles, output_file = "../wb_dg_modern_annotate.pptx")
            #officer::layout_summary(doc.full)
            #layout_properties(doc.full, master = "World Presentation 16x9", layout = "Title Slide")

            slidemaster<-"World Presentation 16x9"

            ## 2.2 DOC TITLE & DATE
            fp_hea<-fp_text(color = "#0d47a1", font.size = 20, bold = T)
            fp_hea_sty<-fp_par(text.align = "center", padding.bottom = 1, keep_with_next = T)

            creatorsusofp<-fpar(ftext(creatorsuso, fp_hea), fp_p = fp_hea_sty)
            doc.full <- doc.full  %>%
              add_slide(master = slidemaster, layout = "Title Slide") %>%
              ph_with(
                value = rep_cont$doc_title[[1]],
                location = ph_location_label("Title 1")
              ) %>%
              ph_with(
                value = rep_cont$doc_title[[2]],
                location = ph_location_label("Subtitle 2")
              )

            ## 2.3. Add Section Para and Tables
            ##    i. loop over section
            ##        ii. loop over para
            incProgress(0.2)
            sldNum<-2
            for (sec_para in names(rep_cont$sec_para)) {
              #incProgress(0.2)
              p<-rep_cont$sec_para[[sec_para]]
              t<-rep_cont$sec_table[[sec_para]]
              i<-rep_cont$sec_graph[[sec_para]]
              sectitle<-rep_cont$sec_title[[sec_para]]$title
              secftr<-rep_cont$sec_title[[sec_para]]$ftr
              secdt<-rep_cont$sec_title[[sec_para]]$fdt
              doc.full <- doc.full  %>%
                add_slide(layout = "Section Header", master = slidemaster) %>%
                ph_with(sectitle, location = ph_location_label("Title 1")) %>%
                ph_with(secftr, location = ph_location_type("ftr"))%>%
                ph_with(secdt, location = ph_location_type("dt"))

              sldNum<-sldNum+1
              for (para in names(p)) {
                pp<-p[[para]]
                tt<-t[[para]]
                ii<-i[[para]]
                ## add para
                if (!is.null(pp)) {
                  doc.full <- doc.full  %>%
                    add_slide(layout = "Comparison", master = slidemaster) %>%
                    ph_with(pp$qte, location = ph_location_label("Title 1")) %>%
                    ph_with(pp$qhea1, location = ph_location_label("Text Placeholder 2")) %>%
                    ph_with(pp$qex, location = ph_location_label("Content Placeholder 3"))%>%
                    ph_with(pp$qhea2, location = ph_location_label("Text Placeholder 4")) %>%
                    ph_with(pp$qex, location = ph_location_label("Content Placeholder 5")) %>%
                    ph_with(pp$qftr, location = ph_location_type("ftr"))%>%
                    ph_with(pp$sldNum, location = ph_location_type("sldNum")) %>%
                    ph_with(pp$qdt, location = ph_location_type('dt'))
                }
                ## add table
                if (!is.null(tt)) {
                  doc.full<-doc.full %>%
                    add_slide(layout = "Title and Content", master = slidemaster) %>%
                    ph_with(tt, location = ph_location_type(type = "body"))

                }
                ## add graph
                if (!is.null(ii)){
                  doc.full<-doc.full %>%
                    add_slide(layout = "Title and Content", master = slidemaster) %>%
                    ph_with(ii, location = ph_location_type(type = "body"))
                }
                sldNum<-sldNum+1
              }
              ## remove first slide from template
              #doc.full<-doc.full
            }
            ## 2.4. Tempfile
            ##############################
            ## 2. Switch working directory for report
            withr::with_dir(tempdir(),
                            {doc.full %>%
                                print(target = "report_for_download_v1.pptx")}
            )
          })


        }

        ## 2.5. Click DWL button
        shinyjs::click("dwl_report")
      })
      ## Download Report
      output$dwl_report <- downloadHandler(
        filename = function() {
          if(type=="word"){
            paste(fn, "-", stringr::str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".docx", sep="")
          } else if(type=="pptx") {
            paste(fn, "-", stringr::str_remove_all(Sys.time(), "[:space:]|[:punct:]"), ".pptx", sep="")
          }
        },
        content = function(file) {
          withr::with_dir(tempdir(),
                          {if(type=="word"){
                            file.copy("report_for_download_v1.docx", file)
                          } else if(type=="pptx") {
                            file.copy("report_for_download_v1.pptx", file)}
                          }
          )

        }, contentType = NULL)

      ####################FIN SERVER####################################################
    }
  )
}
