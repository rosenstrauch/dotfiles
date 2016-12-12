#!/bin/sh

git filter-branch --commit-filter '
        if [ "$GIT_COMMITTER_NAME" = "rosenstrauch" ];
        then
                GIT_COMMITTER_NAME="discipolo";
                GIT_AUTHOR_NAME="discipolo";
                GIT_COMMITTER_EMAIL="discipolo@105174.no-reply.drupal.org";
                GIT_AUTHOR_EMAIL="discipolo@105174.no-reply.drupal.org";
                git commit-tree "$@";
        else
                git commit-tree "$@";
        fi' HEAD
