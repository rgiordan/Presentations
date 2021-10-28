\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{mitletter3}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{letter}}
\ProcessOptions
\LoadClass{letter}
\usepackage[dvips]{graphics}
\usepackage[businessenvelope]{envlab}

\usepackage{color}
\usepackage[usenames,dvipsnames,svgnames,table]{xcolor}
\definecolor{mitred}{RGB}{128,0,0}

\raggedright

\setlength{\topmargin}{-.80in}
\setlength{\oddsidemargin}{-.25in}
\setlength{\evensidemargin}{-.25in}
\setlength{\textwidth}{7in}
\setlength{\textheight}{8in}
\setlength{\headsep}{1in}
\setlength{\footskip}{1.125in}

\DeclareFixedFont{\boldfont}{OT1}{cmss}{bx}{n}{8}
\DeclareFixedFont{\regularfont}{OT1}{cmss}{m}{n}{8}
 \DeclareFixedFont{\italicfont}{OT1}{cmr}{m}{it}{10}

\renewcommand*{\opening}[1]{\ifx\@empty\fromaddress
  \thispagestyle{firstpage}%
    {\raggedleft\@date\par}%
  \else  % home address
    \thispagestyle{empty}%
    {\raggedleft\begin{tabular}{l@{}}\ignorespaces
      \fromaddress \\*[2\parskip]%
      \@date \end{tabular}\par}%
  \fi
  \vspace{2\parskip}%
  {\raggedright \toname \\ \toaddress \par}%
  \vspace{2\parskip}%
  #1\par\nobreak}
\renewcommand{\ps@headings}{% 
  \let\@oddfoot\@empty\let\@evenfoot\@empty
      \def\@oddhead{\slshape\headtoname{} \ignorespaces\toname
                    \@date
                    \hfil \pagename{} \thepage}%
      \let\@evenhead\@oddhead}
\renewcommand*{\opening}[1]{\ifx\@empty\fromaddress
  \thispagestyle{firstpage}%
    \@date\\%
  \else  % home address
    \thispagestyle{empty}%
    {\raggedleft\begin{tabular}{l@{}}\ignorespaces
      \fromaddress \\*[2\parskip]%
      \@date \end{tabular}\par}%
  \fi
  \vspace{2\parskip}%
  {\raggedright \toname \\ \toaddress \par}%
  \vspace{2\parskip}%
  #1\par\nobreak}
\renewcommand*{\signature}[1]{\def\fromsig{\italicfont #1}}
\newcommand{\officename}{Office Name}
\newcommand{\associateofficename}{Associate office name (optional)}
\newcommand{\roomnumber}{X00-000}
\newcommand{\phonenum}{617.000.0000}
\newcommand{\faxnum}{617.000.0000}
\newcommand{\website}{web.mit.edu/abcdefghijk}
\newcommand{\email}{user@mit.edu}
\newcommand{\person}{Your name}
\newcommand{\ID}{Your title}
\newsavebox{\OfficeStuff}
\newsavebox{\MITlogo}
\newsavebox{\contactinfo}
\newsavebox{\phonestuff}


\sbox{\OfficeStuff}
     {\parbox[t]{7.41cm}{\setlength{\baselineskip}{9.2pt}\boldfont{\officename\\{\color{mitred} \associateofficename}}}}

\sbox{\MITlogo}
     {\parbox[t]{2.19cm}
       {\raisebox{-7mm}{\scalebox{.3}{\includegraphics{mitlogo-clr.eps}}}}}
\sbox{\contactinfo}
     {\parbox[t]{7.37cm}
       {\setlength{\baselineskip}{9.2pt}{\boldfont{\color{mitred} Massachusetts Institute of Technology}}\\
	 \regularfont{77 Massachusetts Avenue, \roomnumber\\
	   Cambridge, Massachusetts 02139-4307}}}
\sbox{\phonestuff}
     {\parbox[t]{7.37cm}{\setlength{\baselineskip}{9.2pt}\regularfont{Phone\hspace{2mm}\phonenum\\Fax\hspace{5.5mm}\faxnum\\Email\hspace{2mm} \email\\\website}}}
\newsavebox{\nameandtitle}
\sbox{\nameandtitle}
     {\parbox[t]{3in}{\setlength{\baselineskip}{9.2pt}\boldfont{\person }\\\regularfont{\ID }}}
\newsavebox{\header}
\arrayrulecolor{mitred}
\setlength{\arrayrulewidth}{2pt}
\sbox{\header}
     {\parbox[t]{\textwidth}
       {\begin{tabular*}{\textwidth}{lll@{\extracolsep{\fill}}}
	 \usebox{\OfficeStuff} & \raisebox{.05in}{\usebox{\MITlogo}} & \usebox{\contactinfo} \\
	 &&\\
	 \hline\\
	 \usebox{\nameandtitle}&&\usebox{\phonestuff}
     \end{tabular*}}}

\newsavebox{\footer}

\sbox{\footer}
     {\parbox[t]{\textwidth}{\color{mitred} \rule{\textwidth}{3mm}}}
\newsavebox{\envpic}
\sbox{\envpic}
     {\parbox[t]{.75in}
       {\scalebox{.33}{\includegraphics{mitlogo-clr.eps}}}}
\newsavebox{\envtop}
\sbox{\envtop}
     {\parbox[t]{2.5in}
       {\setlength{\baselineskip}{9.2pt}\boldfont{\officename\\\associateofficename}}}

\newsavebox{\envbottom}
\sbox{\envbottom}
     {\parbox[b]{2.5in}
       {\setlength{\baselineskip}{9.2pt}\boldfont{Massachusetts Institute of Technology}\\\regularfont{77 Massachusetts Avenue, Building \roomnumber}\\\regularfont{Cambridge, MA 02139-4307}}}

\newsavebox{\envelope}
\sbox{\envelope}
     {\parbox[t]{3.25in}
       {\hspace{.75in}\raisebox{.125in}{\usebox{\envtop}}\\\textcolor{mitred}{\rule{3.25in}{2pt}}\\\\\usebox{\envpic}\usebox{\envbottom}}}

\renewcommand{\returnaddress}{\protect\usebox{\envelope}}




%make sure the header goes on the first page
\renewcommand{\ps@firstpage}
	     {\setlength{\headheight}{70pt}\setlength{\headsep}{25pt}%
	       \renewcommand{\@oddhead}{\usebox{\header}}%
	       \renewcommand{\@evenhead}{}\renewcommand{\@evenfoot}{}\renewcommand{\@oddfoot}{\usebox{\footer}}}

\providecommand{\@evenhead}{}\providecommand{\@oddhead}{}
\providecommand{\@evenfoot}{}\providecommand{\@oddfoot}{}

\pagestyle{headings}
