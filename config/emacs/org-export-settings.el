  (setq org-export-backends (quote (reveal ascii html icalendar latex md odt)))


  (setq org-export-in-background t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-latex-classes
        '(("beamer"
           "\\documentclass[presentation]{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("article"
           "\\documentclass[12pt]{article}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  [NO-EXTRA]
  \\settextfraction{0.95}\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("report"
           "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("book"
           "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("une-article"
           "\\documentclass[a4paper,12pt]{scrartcl}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage[margin=1.5cm]{geometry}
  [EXTRA]\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("une-logo"
           "\\documentclass[a4paper,12pt]{scrartcl}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage[margin=1.5cm]{geometry}
  [EXTRA]
  \\definecolor{unegreen}{HTML}{7AB800}
  \\definecolor{Black}{HTML}{000000}
  \\definecolor{White}{HTML}{FFFFFF}
  \\definecolor{dimgrey}{HTML}{696969}
  \\makeatletter
  \\def\\@maketitle{
   \\noindent \\begin{minipage}[c][4cm][t]{\\linewidth}
     \\colorbox{Black}{%
       \\begin{minipage}[t][4cm][c]{4cm}
       \\flushleft
       \\includegraphics{~/org/07-needs/data/e2/d93aae-bf9f-4f84-9cbb-d58618144046/luis_rosenstrauch_s.png}
     \\end{minipage}}
     \\colorbox{unegreen}{%
       \\begin{minipage}[t][4cm][c]{13.5cm}
         \\flushright
         \\Large \\textbf{\\color{White}{\\@title}} \\\\
          \\vspace{4pt}
         \\small \\color{White}{\\@author} \\\\
         \\small \\color{White}{\\@date}
       \\end{minipage}}
     \\end{minipage}}
  \\makeatother\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("old-article" "\\documentclass[11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
        )
;;; *** ORG Config: cleanup after export
  (setq-default org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
  (setq-default org-latex-remove-logfiles t)

