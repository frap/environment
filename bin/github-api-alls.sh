#!/usr/bin/env bash

# Purpose
# grab the jwt token
# make API calls as the GitHub App used

# get a temporary jwt token from the key file and app id (hardcoded in the file:)
generated_jwt=$(./github-app-jwt.sh)
github_api_url="https://api.github.com/app"

installation_id=21043970
org="devops-actions"

# show the jwt during testing
echo "Generated jwt:"
echo "${generated_jwt}"
echo ""

# call the urls with it
echo "Calling [${github_api_url}], result:"
curl -s \
    -H "Authorization: Bearer ${generated_jwt}" \
    -H "Accept: application/vnd.github.machine-man-preview+json" \
    "${github_api_url}"

github_api_url="https://api.github.com/app/installations"
echo "Calling [${github_api_url}], result:"
curl -s \
    -H "Authorization: Bearer ${generated_jwt}" \
    -H "Accept: application/vnd.github.v3+json" \
    "${github_api_url}"

# get the token by POSTING to the url:
github_api_url="https://api.github.com/app/installations/$installation_id/access_tokens"
echo "Calling [${github_api_url}], result:"
tokens=$(curl -s -X POST \
    -H "Authorization: Bearer ${generated_jwt}" \
    -H "Accept: application/vnd.github.v3+json" \
    "${github_api_url}" )
echo "Token info: $tokens"

# extract the token, more information about expiry for example is present as well:
token=$(echo "$tokens" | jq -r '.token')
echo "Token: $token"

# from now until the token expires, you can use the token to make authenticated requests to the GitHub API:
# get the repositories this token has access to
github_api_url="https://api.github.com/installation/repositories"
echo "Calling [${github_api_url}], result:"
curl -s GET \
    -H "Authorization: Bearer ${token}" \
    -H "Accept: application/vnd.github.v3+json" \
    "${github_api_url}"
                
# get the runner information for a repo
github_api_url="https://api.github.com/repos/rajbos/dotnetcore-webapp/actions/runners"
echo "Calling [${github_api_url}], result:"
curl -s \
    -H "Authorization: Bearer ${token}" \
    -H "Accept: application/vnd.github.machine-man-preview+json" \
    "${github_api_url}"

# load the files in a directory
github_api_url="https://api.github.com/repos/rajbos/dotnetcore-webapp/contents/.github/workflows"
echo "Calling [${github_api_url}], result:"
curl -s \
    -H "Authorization: Bearer ${token}" \
    -H "Accept: application/vnd.github.v3+json" \
    "${github_api_url}"

# load a file in a directory
github_api_url="https://api.github.com/repos/rajbos/dotnetcore-webapp/contents/README.md"    
echo "Calling [${github_api_url}], result:"
curl -i -X GET \
    -H "Authorization: Bearer ${token}" \
    -H "Accept: application/vnd.github.v3+json" \
    "${github_api_url}"
