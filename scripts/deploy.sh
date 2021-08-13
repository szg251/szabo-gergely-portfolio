#!/bin/bash

if [[ $ENV == "prod" ]]
then
  echo "Deploying to Vercel..."
	vercel --prod

  echo "Deploying to IPFS..."
  cd result
  ipfs name publish \
    --key=SzaboGergelyPortfolioKey \
    /ipfs/$( ipfs add -r . | grep szabo-gergely-portfolio$ | awk '{print $2}' )
  cd ../
else
	vercel
fi
