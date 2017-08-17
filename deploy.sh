echo "1. build ai"
cd ai
./build.sh
cd ..
echo "2. deploy serverless"
serverless deploy function -f move
