#!/bin/bash
set -e

# Script to query available Shopify API versions (Admin or Storefront)
# Usage: ./scripts/get-shopify-api-versions.sh <env-file> <api-type>
# Example: ./scripts/get-shopify-api-versions.sh .env.api admin

if [ $# -lt 2 ]; then
  echo "Usage: $0 <env-file> <api-type>" >&2
  echo "" >&2
  echo "Arguments:" >&2
  echo "  env-file  - Path to environment file" >&2
  echo "  api-type  - Either 'admin' or 'storefront'" >&2
  echo "" >&2
  echo "Example: $0 .env.api admin" >&2
  exit 1
fi

ENV_FILE="$1"
API_TYPE="$2"

if [ ! -f "$ENV_FILE" ]; then
  echo "Error: Environment file '$ENV_FILE' not found" >&2
  exit 1
fi

if [ "$API_TYPE" != "admin" ] && [ "$API_TYPE" != "storefront" ]; then
  echo "Error: api-type must be 'admin' or 'storefront', got: '$API_TYPE'" >&2
  exit 1
fi

# Load environment variables
set -a
source "$ENV_FILE"
set +a

# Configure based on API type
ACCESS_TOKEN=""
SHOP_URL=""
CURRENT_VERSION=""
HEADER_NAME=""
API_PATH=""

if [ "$API_TYPE" = "admin" ]; then
  # Admin API configuration
  if [ -z "$SHOPIFY_ADMIN_API_TOKEN" ]; then
    echo "Error: SHOPIFY_ADMIN_API_TOKEN not found in $ENV_FILE" >&2
    exit 1
  fi
  if [ -z "$SHOPIFY_SHOP_NAME" ]; then
    echo "Error: SHOPIFY_SHOP_NAME not found in $ENV_FILE" >&2
    exit 1
  fi
  if [ -z "$SHOPIFY_API_VERSION" ]; then
    echo "Error: SHOPIFY_API_VERSION not found in $ENV_FILE" >&2
    exit 1
  fi

  ACCESS_TOKEN="$SHOPIFY_ADMIN_API_TOKEN"
  SHOP_URL="https://$SHOPIFY_SHOP_NAME"
  CURRENT_VERSION="$SHOPIFY_API_VERSION"
  HEADER_NAME="X-Shopify-Access-Token"
  API_PATH="/admin/api"

else
  # Storefront API configuration
  if [ -z "$STOREFRONT_ACCESS_TOKEN" ]; then
    echo "Error: STOREFRONT_ACCESS_TOKEN not found in $ENV_FILE" >&2
    exit 1
  fi
  if [ -z "$GRAPHQL_URL" ]; then
    echo "Error: GRAPHQL_URL not found in $ENV_FILE" >&2
    exit 1
  fi

  ACCESS_TOKEN="$STOREFRONT_ACCESS_TOKEN"
  HEADER_NAME="X-Shopify-Storefront-Access-Token"
  API_PATH="/api"

  # Extract store URL from GRAPHQL_URL
  if [[ "$GRAPHQL_URL" =~ ^https?:// ]]; then
    SHOP_URL=$(echo "$GRAPHQL_URL" | sed -E 's|(https?://[^/]+)/.*|\1|')
  else
    echo "Error: GRAPHQL_URL must be a full URL (starting with https://) in $ENV_FILE" >&2
    echo "Found: $GRAPHQL_URL" >&2
    exit 1
  fi

  # Extract current API version from GRAPHQL_URL
  CURRENT_VERSION=$(echo "$GRAPHQL_URL" | grep -oE '[0-9]{4}-[0-9]{2}')

  if [ -z "$CURRENT_VERSION" ]; then
    echo "Error: Could not extract API version from GRAPHQL_URL" >&2
    exit 1
  fi
fi

echo "Querying Shopify $API_TYPE API versions from $SHOP_URL (current: $CURRENT_VERSION)..." >&2
echo "" >&2

# Query publicApiVersions
RESPONSE=$(curl -s -X POST \
  "$SHOP_URL$API_PATH/$CURRENT_VERSION/graphql.json" \
  -H "Content-Type: application/json" \
  -H "$HEADER_NAME: $ACCESS_TOKEN" \
  -d '{"query": "{ publicApiVersions { handle displayName supported } }"}')

# Check for errors
if echo "$RESPONSE" | jq -e '.errors' > /dev/null 2>&1; then
  echo "Error querying API:" >&2
  echo "$RESPONSE" | jq '.errors' >&2
  exit 1
fi

# Pretty print the results
echo "$RESPONSE" | jq -r '
  .data.publicApiVersions[] |
  "\(.handle)\t\(.displayName)\t\(if .supported then "✓ supported" else "✗ unsupported" end)"
' | column -t -s $'\t'

echo "" >&2

# Identify latest stable and recommended version
LATEST_STABLE=$(echo "$RESPONSE" | jq -r '.data.publicApiVersions[] | select(.displayName | contains("(Latest)")) | .handle')
VERSIONS=($(echo "$RESPONSE" | jq -r '.data.publicApiVersions[] | select(.supported == true) | .handle' | sort -V))

# Find the version before latest stable
RECOMMENDED=""
for i in "${!VERSIONS[@]}"; do
  if [ "${VERSIONS[$i]}" = "$LATEST_STABLE" ] && [ $i -gt 0 ]; then
    RECOMMENDED="${VERSIONS[$i-1]}"
    break
  fi
done

echo "Current version:     $CURRENT_VERSION" >&2
echo "Latest stable:       $LATEST_STABLE" >&2
if [ -n "$RECOMMENDED" ]; then
  echo "Recommended target:  $RECOMMENDED (one behind latest)" >&2
fi
