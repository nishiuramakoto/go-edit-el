;; generate command line parser

;; general utilities
(defun my-zip (f x y)
  (let* ( (z (list nil))  (zi z) ) 
    (while (and x y) 
      (nconc zi (list (funcall f (car x) (car y))))
      (setq zi (cdr zi))
      (setq x (cdr x))
      (setq y (cdr y)))
    (cdr z)))


(defun my-uniq (l &optional pred)
  (setq pred (or pred 'equal))
  (cons (car l) 
	(apply 'append
	       (my-zip  (lambda (x y) (unless (funcall pred x y) (list y)))
		     l (cdr l)))))
(defun my-filter (l &optional p)
  (setq p (or p 'identity))
  (apply 'append (mapcar (lambda (x) (when (funcall p x) (list x))) l)))


(defvar gen-main-default-version
  '( "version" nil
     "output version information and exit"
     opt-action
     "printf (\"%s %s\\n\", PACKAGE,VERSION); exit(0);"))

(defvar gen-main-default-help
  '(  "help"   nil
      "display this help and exit"
      opt-action
      "usage(0);" ))



(defvar gen-main-header-template "

#ifdef __cplusplus
extern \"C\" {
#endif

/* global option variables  */
%s

/*
     This is the user supplied main routine that will be called 
     after parsing command line arguments.
     argc is garranteed to be >0, and argv[0] contains
     what one normally expects in the standard entry function  'int main(int argc,char** argv)'.
     This way you can seemlessly (just by changing the name of the entry function)
     switch between the option-parsed version and the non-option version of the program.

     Parameters 
     argc   : 1 + the number of non-option arguments
     argv   : Array  of non-option arguments.
              argv[0] contains the path with which the program was invoked.

     Retuns : Should return 0 on success, otherwise appropriate 
              non-zero error status. 
*/
int %s (int argc,char**argv);

#ifdef __cplusplus
} // extern \"C\" {
#endif

")


(defvar gen-main-code-template "
#include <stdio.h>
#include <getopt.h>
#include <assert.h>
#include <stdlib.h>

#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif


#ifndef PACKAGE 
#define PACKAGE \"@PACKAGE@\"
#endif

#ifndef VERSION 
#define VERSION \"@VERSION@\"
#endif



/*  option variable declarations here */
%s
/*  end  variable declarations here */

static void usage (int status);

/* The name the program was run with, stripped of any leading path. */
static char *program_name;

/* Option flags and variables */


static struct option const long_options[] =
{
/* long_options members here */
%s
/* long_options members end here */
  {NULL		, 0, NULL, 0}
};


static int decode_switches (int argc, char **argv);

int
main (int argc, char **argv)
{
  int i;

  program_name = argv[0];

  i = decode_switches (argc, argv);

  /* do the work */
  {
    char* tmp= argv[i-1];
    argv[i-1] = argv[0];
    argv[0] = tmp;
  }
  return %s (argc-(i-1),&argv[i-1]);
}


/* Set all the option flags according to the switches specified.
   Return the index of the first non-option argument.  */

static int
decode_switches (int argc, char **argv)
{
  int c;
  int long_option_index;
  while ((c = getopt_long (argc, argv, 
/*  short option spec here */
\"%s\"
/*  short option spec end here */
			   ,
			   long_options, &long_option_index )) != EOF)
    {
      switch (c){
		case 0:
        /* handle long option */
	switch (long_option_index) {
/* long option handler */
%s         
/* long option handler end */
		default:
		  usage (EXIT_FAILURE);
	}
	break;

/*  short option actions start here */
%s
/*  short option actions end  here */
		default:
		  usage (EXIT_FAILURE);
       } // switch(c)
    } // while
  return optind;
}

static void
usage (int status)
{
/* program description  here */
  printf(\"%%s - %s\\n\",program_name);
/*  program synopsis here */
  printf(\"Usage: %%s %s\\n\",program_name);

  printf (\"\\
Options:\\n\\
%s\");
  exit (status);
}

")

(defvar gen-main-argtype
      '((opt-string . "STRING")  
	(opt-int    .  "INT") 
	(opt-bool   . "[INT]")
	(opt-float  . "FLOAT")
	))


(defun gen-main-split-var (var)
  (when   (string-match "^[[:space:]]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)[[:space:]]*\\(=\\)?" var)
    (let ((vn  (match-string 1 var))
	  (def (match-string 2 var))
	  (end (match-end 0)))
      (if def 
	  (list vn (substring var end ))
	(list vn)))))

(defun gen-main-varname (opt)
  (car (gen-main-split-var (nth 4 opt))))

(defun gen-main-mk-var-list (opts)
  (mapcar (lambda (opt)  
	    (let ((type (nth 3 opt))
		  (var  (nth 4 opt)))
	      (cond
	       ((eq type 'opt-string)  (append  '("char*  ")  (gen-main-split-var var)))
	       ((eq type 'opt-int)     (append  '("int    ")  (gen-main-split-var var)))
	       ((eq type 'opt-bool)    (append  '("int    ")  (gen-main-split-var var)))
	       ((eq type 'opt-float)   (append  '("double ")  (gen-main-split-var var)))
	       ((eq type 'opt-action)  nil)
	       (t                      (error (format "unknown type : %S" type)))
	       ))) opts))


(defun gen-main-mk-ext-var-decl (opts)
  (let ((var-list (gen-main-mk-var-list opts)))
    (apply 'concat (mapcar 
		    (lambda (v) (when v (concat "extern " (nth 0 v ) (nth 1 v ) ";\n"))) 
		    var-list))))

(defun gen-main-mk-var-decl (opts)
  (let ((var-list (gen-main-mk-var-list opts)))
    (apply 'concat (mapcar 
		    (lambda (v) 
		      (when v (concat (nth 0 v ) (nth 1 v )  "="  (or (nth 2 v) "0") ";\n")))
		    var-list))))


(defun gen-main-mk-long-opt-each (opt)
  (let* ((long  (nth 0 opt))
	 (short (nth 1 opt))
	 (type  (nth 3 opt))
	 (arg-type
	  (cond
	   ((eq type 'opt-string)  "required_argument")
	   ((eq type 'opt-int)     "required_argument")
	   ((eq type 'opt-float)   "required_argument")
	   ((eq type 'opt-bool)    "optional_argument")
	   ((eq type 'opt-action)  "no_argument"))))
    (when long
      (if short
	  (format "\t{\"%s\"	,%s	,NULL		,'%s'},\n" long arg-type (string short))
	(format "\t{\"%s\"	,%s	,NULL		,0},\n" long arg-type)))))


(defun gen-main-mk-long-opt  (opts)
  (apply 'concat  (mapcar 'gen-main-mk-long-opt-each  opts)))

(defun gen-main-mk-short-opts-each (opt)
  (let  ((short (nth 1 opt))
	 (type  (nth 3 opt)))
    (when short
      (cond
       ((eq type 'opt-string)  (string short ?:))
       ((eq type 'opt-int)     (string short ?:))
       ((eq type 'opt-float)   (string short ?:))
       ((eq type 'opt-bool)    (string short ?: ?:))
       ((eq type 'opt-action)  (string short ))))))

(defun gen-main-mk-short-opts (opts)
  (apply 'concat (mapcar 'gen-main-mk-short-opts-each opts)))

	   
(defun gen-main-mk-action-each (opt)
  (let*  ((type  (nth 3 opt))
	  (code  (if (eq type 'opt-action) 
		     (nth 4 opt) 
		   (nth 5 opt))))
    (unless code (setq code ""))
    (cond
     ((eq type 'opt-string)  (format "%s = optarg;%s"  			(gen-main-varname opt) code))
     ((eq type 'opt-int)     (format "%s = strtol(optarg,NULL,0);%s;" 	(gen-main-varname opt) code))
     ((eq type 'opt-float)   (format "%s = strtod(optarg,NULL);%s;" 	(gen-main-varname opt) code))
     ((eq type 'opt-bool)    (format "%s = optarg ? strtol(optarg,NULL,0) : 1 ;%s;"   (gen-main-varname opt) code))
     ((eq type 'opt-action)  code))))

(defun gen-main-mk-action-long (opts)
  (let ((c -1))
    (apply 'concat
	   (mapcar (lambda (opt) 
		     (setq c (+ c 1))
		     (let ((long  (nth 0 opt)))
		       (when long
			 (format "\tcase %d: { 
assert(!strcmp(long_options[long_option_index].name , \"%s\")) ;
 %s ; break;}\n" 
				 c long (gen-main-mk-action-each opt)))))
		   opts))))

(defun gen-main-mk-action-short (opts)
  (apply 'concat
	 (mapcar (lambda (opt)
		   (let ((short (nth 1 opt)))
		     (when short
		       (format "\tcase '%c': { %s ; break; }\n"  short (gen-main-mk-action-each opt)))))
		 opts)))

(defun gen-main-mk-usage-each (opt)
  (let*  ((long  (nth 0 opt))
	  (short (nth 1 opt))
	  (usage (nth 2 opt))
	  (type  (nth 3 opt))
	  (opt-form (cond 
		     ((and long short) (format "-%c, --%-8s" short long))
		     (long             (format "    --%-8s"       long))
		     (short            (format "-%c      " short))
		     (t (error "either short or long option should be specified"))))
	  (opt-arg (or (cdr (assq type gen-main-argtype )) "" )))
    (format "  %-12s\t%s\t\t%s\\n\\\n" opt-form opt-arg usage)))

  

(defun gen-main (user-main desc synopsis  opts)
  (list  
   (format gen-main-header-template 
	   (gen-main-mk-ext-var-decl opts) 	
	   user-main)

   (format gen-main-code-template 
	   (gen-main-mk-var-decl opts)
	   (apply 'concat (mapcar 'gen-main-mk-long-opt-each opts))
	   user-main
	   (gen-main-mk-short-opts opts)
	   (gen-main-mk-action-long opts)
	   (gen-main-mk-action-short opts)
	   desc
	   synopsis
	   (apply 'concat (mapcar 'gen-main-mk-usage-each opts))
	   )))

(defun gen-main-mk-header-guard (path)
  (let ((file (file-name-nondirectory path)))
    (apply 'concat (mapcar (lambda (x) (concat  "__" x))  (split-string  file "[^[:alnum:]_]+")))))
	

(defun gen-main-gnuify (opt-spec)
  (let* ((user-main  (nth 0 opt-spec))
	 (desc       (nth 1 opt-spec))
	 (synop      (nth 2 opt-spec))
	 (opts       (nth 3 opt-spec))
	 (help       (assoc "help"    opts))
	 (version    (assoc "version" opts))
	 (long-opts        (sort (my-filter (mapcar 'car opts)) 'string<))
	 (uniq-long-opts   (my-uniq long-opts))
	 (short-opts       (sort (my-filter (mapcar 'cadr opts)) '<))
	 (uniq-short-opts  (my-uniq short-opts)))
	 
    (unless (stringp user-main)
      (error "specify user-main as the first member"))
    (unless (stringp desc)
      (error "specify description as the second member"))
    (unless (stringp synop)
      (error "specify synopsis  as the third member"))

    (unless (eq (length long-opts) (length uniq-long-opts))
      (error "there are duplicate long options"))
    (unless (eq (length short-opts) (length uniq-short-opts))
      (error "there are duplicate short options"))

    (unless version
      (setq opts (cons  gen-main-default-version opts)))
    (unless help
      (setq opts (cons  gen-main-default-help opts)))
    (list user-main desc synop opts)))

(defun gen-main-write (opt-spec main-file header-file)
  (let* ((codes  (apply 'gen-main (gen-main-gnuify opt-spec)))
	 (header-code (nth 0 codes))
	 (main-code   (nth 1 codes))
	 (header-guard (gen-main-mk-header-guard header-file)))
    (with-temp-file header-file
      (insert (format "#ifndef %s\n#define %s\n"  header-guard header-guard)
	      header-code
	      (format "#endif //  %s\n"  header-guard)))
    (with-temp-file main-file
      (insert  (format "#include \"%s\"\n" (file-name-nondirectory header-file))
	       main-code))))
    

