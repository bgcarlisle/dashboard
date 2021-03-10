about_rm_page <- tabPanel(
    "About Responsible Metrics",
    value = "tabAboutRM",
    h3("Contributions"),
    br(),
    helpText('We would like to thank all those who contributed to this proof-of-principle dashboard. We would
             particularly like to thank Nico Riedel who designed an initial version of this dashboard, which
             we adapted for use with multiple institutions.'),
    br(),
    h4("UMC publication search"),
    helpText('Franzen, Delwen (Conceptualization, Methodology, Technical Implementation, Validation); Saksone, Lana (Conceptualization, Methodology, Validation); Grabitz, Peter (Conceptualization, Methodology); Riedel, Nico (Conceptualization, Methodology, Technical Implementation); Carlisle, Benjamin Gregory (Methodology, Technical Implementation, Validation), Holst, Martin (Conceptualization, Validation); Salholz-Hillel, Maia (Conceptualization, Methodology); Strech, Daniel (Conceptualization, Methodology)'),
    br(),
    h4("ODDPub - Open Data & Code detection"),
    helpText('Riedel, Nico (Conceptualization, Methodology, Technical Implementation, Validation);
                                Bobrov, Evgeny (Conceptualization, Methodology, Validation);
                                Kip, Miriam (Conceptualization, Methodology)'),
    br(),
    h4("Clinical trial metrics"),
    h5("IntoValue"),
    helpText("Riedel, Nico (Conceptualization, Methodology, technical implementation);
             Strech, Daniel (Conceptualization, Methodology);
             Wieschowski, Susanne (Conceptualization, Methodology)"),
    h5("Reporting of trial registration number"),
    helpText("Salholz-Hillel, Maia (Conceptualization, Methodology);
             Carlisle, Benjamin Gregory (Conceptualization, Methodology)"),
    
    br(),
    h4("Robustness of animal studies"),
    helpText('We thank Anita Bandrowski for sharing with us SciScore data from which we derived the robustness
             metrics in animal studies displayed in this proof-of-principle dashboard'),
    
    
    br(),
    h4("Shiny app"),
    helpText('Riedel, Nico (Conceptualization, Technical Implementation);
                                Carlisle, Benjamin Gregory (Conceptualization, Technical Implementation);
                                Franzen, Delwen (Conceptualization);
                                Maia Salholz-Hillel (Conceptualization);
                                Holst, Martin (Conceptualization);
                                Haven, Tamarinde (Conceptualization);
                                Haslberger, Martin (Conceptualization);
                                Saksone, Lana (Conceptualization);
                                Strech, Daniel (Conceptualization);
                                Weissgerber, Tracey (Conceptualization);
                                Dirnagl, Ulrich (Conceptualization);
                                Bobrov, Evgeny (Conceptualization)'),
    
    br(),
    h3('Contact address'),
    helpText('QUEST Center for Transforming Biomedical Research,'),
    helpText('Berlin Institute of Health (BIH), Berlin, Germany'),
    helpText('Anna-Louisa-Karsch-Str. 2'),
    helpText('10178 Berlin '),
    helpText('quest@bihealth.de'),
    helpText(HTML('<a href="https://www.bihealth.org/quest-center/">
                                      https://www.bihealth.org/quest-center/ </a>'))
)

