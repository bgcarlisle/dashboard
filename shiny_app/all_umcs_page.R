allumc_openaccess_tooltip <- strwrap("The Open Access metric shows the percentage of research publications that are
                             published as Open Access (OA) articles. Gold OA denotes publication in a pure
                             OA journal. Green OA denotes a freely available repository version. Hybrid OA
                             denotes an OA publication in a journal with offers both a subscription based
                             model as well as an Open Access option. For some articles no Open Access
                             information was available.")

allumc_greenoa_tooltip <- strwrap("")

allumc_opendata_tooltip <- strwrap("The Open Data metric measures the percentage of publications in English and for
                            which the full text could be screened that mention sharing of data.
                            Openly shared data makes research more transparent, as research findings can be
                            reproduced. Additionally, shared datasets can be reused and combined by other
                            scientists to answer new research questions.")

allumc_opencode_tooltip <- strwrap("The Open Code metric measures the percentage of publications in English and for
                            which the full text could be screened that mention sharing of code.
                            Like openly shared data, Open Code makes research more transparent, as research
                            findings can be reproduced.")

allumc_clinicaltrials_trn_tooltip <- strwrap("This metric measures how many publications classified by PubMed as clinical trial
                        publications report a trial registration number (TRN) in the abstract and in the
                        secondary identifier metadata of the publication. Reporting of clinical trial registration
                        numbers in related publications facilitates transparent linkage between registration and
                        publication. The CONSORT as well as the ICMJE Recommendations for the Conduct, Reporting,
                        Editing, and Publication of Scholarly Work in Medical Journals call for reporting
                        <i>&#39trial registration number and name of the trial register&#39</i> in both the
                       full-text and abstract.")

allumc_clinicaltrials_sumres_tooltip <- strwrap("This metric measures how many clinical trials registered in the
                        EU Clinical Trials Register that are due to report their results have already
                        done so. A trial is due to report its results 12 month after trial completion.
                        The data were retrieved from the EU Trials Tracker by the EBM DataLab
                        (eu.trialstracker.net). Clinical trials are expensive and have often many contributing
                        patients. A fast dissemination of the trial results is crucial to make the evidence
                        gained in those trials available. The World Health organization recommends publishing
                        clinical trial results within one year after the end of a study.")

allumc_clinicaltrials_prereg_tooltip <- strwrap("This metric measures if the clinical trials are registered before the
                        start date of the study, according to the information given on ClinicalTrials.gov.
                        The idea of prospective registration of studies is to make the trial specifications,
                        including primary and secondary outcomes, publicly available before study start.
                        Prospective registration adds transparency, helps protect against outcome switching.")

allumc_clinicaltrials_timpub_tooltip <- strwrap("This metric measures how many clinical trials registered on ClinicalTrials.gov
                        reported their results either as a journal publication or as summary results on the
                        trials registry within 2 or 5 years after completion. Trials completed between 2009
                        and 2013 were considered. The results were previously published as part of the
                        IntoValue study (https://s-quest.bihealth.org/intovalue/). Clinical trials are expensive
                        and often have many contributing patients. A fast dissemination of the trial results
                        is crucial to make the evidence gained in those trials available. The World Health
                        organization recommends publishing clinical trial results within one year after the
                        end of a study.")

allumc_animal_rando_tooltip <- strwrap("This metric measures how many animal studies report a statement on
                            randomization of subjects into groups. Animal studies were identified using a
                            previously published PubMed search filter. Reporting of randomization was evaluated
                            with SciScore, an automated tool which evaluates research articles based on their
                            adherence to rigour and reproducibility criteria. Only publications in the
                            PubMed Central corpus and which could be analyzed by SciScore are were included
                            in this analysis.")

allumc_animal_blind_tooltip <- strwrap("This metric measures how many animal studies report a statement on whether
                            investigators were blinded to group assignment and/or outcome assessment. Animal
                            studies were identified using a previously published PubMed search filter. Reporting
                            of blinding was evaluated with SciScore, an automated tool which evaluates research
                            articles based on their adherence to rigour and reproducibility criteria. Only
                            publications in the PubMed Central corpus and which could be analyzed by SciScore
                            are were included in this analysis.")

allumc_animal_power_tooltip <- strwrap("This metric measures how many animal studies report a statement on sample size
                         calculation. Animal studies were identified using a previously published PubMed search
                         filter. Reporting of sample size calculation was evaluated with SciScore, an automated
                         tool which evaluates research articles based on their adherence to rigour and
                         reproducibility criteria. Only publications in the PubMed Central corpus and which
                         could be analyzed by SciScore are were included in this analysis.")

#allumc_animal_iacuc_tooltip <- strwrap("This metric ...")

## Define the page layout
all_umcs_page <- tabPanel(
    "All UMCs", value = "tabAllUMCs",
    wellPanel(
        br(),
        fluidRow(
            column(
                12,
                h1(
                    style = "margin-left: 0",
                    strong("Proof-of-principle Responsible Metrics Dashboard: All UMCs"),
                    align = "left"
                ),
                h4(
                    style = "margin-left: 0",
                    "This dashboard provides an overview of the relative performance of several German University Medical Centres (UMCs) on several metrics of open and responsible research. For more detailed information on the methods used to calculate those metrics, view the Methods page."
                ),
                h4(style = "margin-left:0cm",
                   "This dashboard is a pilot that is still under development, and should not be used to compare UMCs or inform policy. More metrics may be added in the future."),
                br()
            )
        )
    ),
    uiOutput("allumc_registration"),
    uiOutput("allumc_reporting"),
    uiOutput("allumc_openscience"),
    ## uiOutput("allumc_robustness")
)
