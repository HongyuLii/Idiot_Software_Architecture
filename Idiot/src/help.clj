(ns help
  (:require compute_address_blob))

(defn top-level []

  (println "idiot: the other stupid content tracker\n")
  (println "Usage: idiot [<top-args>] <command> [<args>]\n")
  (println "Top-level arguments:")
  (println "   -r <dir>   run from the given directory instead of the current one")
  (println "   -d <dir>   store the database in <dir> (default: .idiot)\n")
  (println "Commands:")
  (println "   branch [-d <branch>]")
  (println "   cat-file {-p|-t} <address>")
  (println "   commit <tree> -m \"message\" [(-p parent)...]")
  (println "   commit-tree <tree> -m \"message\" [(-p parent)...]")
  (println "   explore [-p <port>]")
  (println "   hash-object [-w] <file>")
  (println "   help")
  (println "   init")
  (println "   log --oneline [-n <count>] [<ref>]")
  (println "   rev-list [-n <count>] [<ref>]")
  (println "   rev-parse <ref>")
  (println "   switch [-c] <branch>")
  (println "   write-wtree"))

(defn help-instructions-help []
  (println "idiot help: print help for a command\n")
  (println "Usage: idiot help <command>\n")
  (println "Arguments:")
  (println "   <command>   the command to print help for\n")
  (println "Commands:")
  (println "   branch [-d <branch>]")
  (println "   cat-file {-p|-t} <address>")
  (println "   commit <tree> -m \"message\" [(-p parent)...]")
  (println "   commit-tree <tree> -m \"message\" [(-p parent)...]")
  (println "   explore [-p <port>]")
  (println "   hash-object [-w] <file>")
  (println "   help")
  (println "   init")
  (println "   log --oneline [-n <count>] [<ref>]")
  (println "   rev-list [-n <count>] [<ref>]")
  (println "   rev-parse <ref>")
  (println "   switch [-c] <branch>")
  (println "   write-wtree"))

(defn help-instructions-init []
  (println "idiot init: initialize a new database\n")
  (println "Usage: idiot init\n")
  (println "Arguments:")
  (println "   -h   print this message"))

(defn help-instructions-hash []
  (println "idiot hash-object: compute address and maybe create blob from file\n")
  (println "Usage: idiot hash-object [-w] <file>\n")
  (println "Arguments:")
  (println "   -h       print this message")
  (println "   -w       write the file to database as a blob object")
  (println "   <file>   the file"))

(defn help-instructions-cat []
  (println "idiot cat-file: print information about an object\n")
  (println "Usage: idiot cat-file {-p|-t} <address>\n")
  (println "Arguments:")
  (println "   -h          print this message")
  (println "   -p          pretty-print contents based on object type")
  (println "   -t          print the type of the given object")
  (println "   <address>   the SHA1-based address of the object"))

(defn help-instructions-write-wtree []
  (println "idiot write-wtree: write the working tree to the database\n")
  (println "Usage: idiot write-wtree\n")
  (println "Arguments:")
  (println "   -h       print this message"))

(defn help-instructions-commit-tree []
  (println "idiot commit-tree: write a commit object based on the given tree\n")
  (println "Usage: idiot commit-tree <tree> -m \"message\" [(-p parent)...]\n")
  (println "Arguments:")
  (println "   -h               print this message")
  (println "   <tree>           the address of the tree object to commit")
  (println "   -m \"<message>\"   the commit message")
  (println "   -p <parent>      the address of a parent commit"))

(defn help-instructions-rev []
  (println "idiot rev-parse: determine which commit a ref points to\n")
  (println "Usage: idiot rev-parse <ref>\n")
  (println "<ref> can be:")
  (println "- a branch name, like 'master'")
  (println "- literally 'HEAD'")
  (println "- literally '@', an alias for 'HEAD'"))

(defn help-instructions-switch []
  (println "idiot switch: change what HEAD points to\n")
  (println "Usage: idiot switch [-c] <branch>\n")
  (println "Arguments:")
  (println "   -c   create the branch before switching to it"))

(defn help-instructions-branch []
  (println "idiot branch: list or delete branches\n")
  (println "Usage: idiot branch [-d <branch>]\n")
  (println "Arguments:")
  (println "   -d <branch>   delete branch <branch>"))

(defn help-instructions-commit []
  (println "idiot commit: create a commit and advance the current branch\n")
  (println "Usage: idiot commit <tree> -m \"message\" [(-p parent)...]\n")
  (println "Arguments:")
  (println "   -h               print this message")
  (println "   <tree>           the address of the tree object to commit")
  (println "   -m \"<message>\"   the commit message")
  (println "   -p <parent>      the address of a parent commit"))

(defn help-instructions-rev-list []
  (println "idiot rev-list: list preceding revisions, latest first\n")
  (println "Usage: idiot rev-list [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn help-instructions-log []
  (println "idiot log: print abbreviated commit addresses and commit summaries\n")
  (println "Usage: idiot log --oneline [-n <count>] [<ref>]\n")
  (println "Arguments:")
  (println "   -n <count>   stop after <count> revisions (default: don't stop)")
  (println "   <ref>        a reference; see the rev-parse command (default: HEAD)"))

(defn help-instructions-explore []
  (println "idiot explore: start a web server to explore the database\n")
  (println "Usage: idiot explore [-p <port>]\n")
  (println "Arguments:")
  (println "   -p <port>   listen on the given port (default: 3000)"))

(defn help [content]
  (let [instruction (first content)]
    (cond (true? (= nil instruction)) (top-level)
          (or (= 0 (compare instruction "--help")) (= 0 (compare instruction "-h"))) (help-instructions-help)
          (= 0 (compare instruction "init")) (help-instructions-init)
          (= 0 (compare instruction "hash-object")) (help-instructions-hash)
          (= 0 (compare instruction "cat-file")) (help-instructions-cat)
          (= 0 (compare instruction "write-wtree")) (help-instructions-write-wtree)
          (= 0 (compare instruction "commit-tree")) (help-instructions-commit-tree)
          (= 0 (compare instruction "help")) (help-instructions-help)
          (= 0 (compare instruction "rev-parse")) (help-instructions-rev)
          (= 0 (compare instruction "switch")) (help-instructions-switch)
          (= 0 (compare instruction "branch")) (help-instructions-branch)
          (= 0 (compare instruction "commit")) (help-instructions-commit)
          (= 0 (compare instruction "rev-list")) (help-instructions-rev-list)
          (= 0 (compare instruction "log --oneline")) (help-instructions-log)
          (= 0 (compare instruction "explore")) (help-instructions-explore)
          :else (println

                 "Error: invalid command"))))





