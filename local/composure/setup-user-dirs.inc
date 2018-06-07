setup-user-dirs ()
{
    author 'rosenstrauch'
    about 'setup user dirs'
    param ''
    example 'setup-user-dirs'
    group 'setup'
    xdg-user-dirs-update --set VIDEOS $MYDATADIR/DataLib/VIDS
    xdg-user-dirs-update --set DOWNLOAD ${MYDATADIR}/TRANSFER/INCOMING
    xdg-user-dirs-update --set TEMPLATES ${MYDATADIR}/Templates
    xdg-user-dirs-update --set PUBLICSHARE ${MYDATADIR}/TRANSFER/out
    xdg-user-dirs-update --set DOCUMENTS ${MYDATADIR}/DataLib/DOCS
    xdg-user-dirs-update --set MUSIC ${MYDATADIR}/DataLib/AUDIO
    xdg-user-dirs-update --set PICTURES ${MYDATADIR}/DataLib/PICS

}
