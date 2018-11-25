# wersim: Simulating and Measuring Word Error

The wersim package enables researchers to measure word error in documents by comparing a hypothesis corpus (for example obtained from an Automated Speech Recognition Service such as YouTube) to a reference corpus that contains th "correct" text (for example obtained from professional human transcriptions). This is done using the "wer" function.

The package also provides the "wersim" and "wersimtext" functions that let researchers add additional word error to the corpus (through wersim) and then run text models such as Wordfish and sentiment dictionaries on that corpus (with wersimtext).

For details on how to use the functions, please see the function documentations. For a general overview of this project, please see the paper in Political Analysis on which this package is built. It explains why automated transcriptions can be used, how they generally compare to human transcriptions and how wersim provides a way for researchers to test automated transcriptions:

Proksch, S.,  Wratil, C.,  and Wäckerle, J., 2018, Testing the Validity of Automatic Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
