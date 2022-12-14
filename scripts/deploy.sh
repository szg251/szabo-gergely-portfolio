#!/bin/bash

if [[ $ENV == "prod" ]]
then
  # echo "Deploying to Vercel..."
	# vercel --prod

  echo "Deploying to IPFS..."
  cd result
  ipfsHash=$( ipfs add -r . | grep szabo-gergely-portfolio$ | awk '{print $2}' )
  ipfs pin remote add $ipfsHash --service=pinata
  ipfs name publish --key=SzaboGergelyPortfolioKey /ipfs/$ipfsHash
  echo $ipfsHash
  cd ../
else
	vercel
fi
