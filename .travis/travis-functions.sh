#!/bin/env bash
#
# Copyright 2019 - Swiss Data Science Center (SDSC)
# A partnership between École Polytechnique Fédérale de Lausanne (EPFL) and
# Eidgenössische Technische Hochschule Zürich (ETHZ).
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

function publishCharts() {
  make login

  # decrypt ssh key to use for docker hub login
  openssl aes-256-cbc -K $encrypted_b7eb5d86688a_key -iv $encrypted_b7eb5d86688a_iv -in deploy_rsa.enc -out deploy_rsa -d
  chmod 600 deploy_rsa
  eval $(ssh-agent -s)
  ssh-add deploy_rsa

  # build charts/images and push
  cd helm-chart
  chartpress --push --publish-chart
  git diff
  # push also images tagged with "latest"
  chartpress --tag latest --push
}

function createRelease() {
  # fixing git setup
  echo "Fixing git setup for $TRAVIS_BRANCH"
  git checkout ${TRAVIS_BRANCH}
  git branch -u origin/${TRAVIS_BRANCH}
  git config branch.${TRAVIS_BRANCH}.remote origin
  git config branch.${TRAVIS_BRANCH}.merge refs/heads/${TRAVIS_BRANCH}
  git config --global user.name "RenkuBot"
  git config --global user.email "renku@datascience.ch"
  git config credential.helper "store --file=.git/credentials"
  echo "https://${GITHUB_TOKEN}:@github.com" >.git/credentials

  # releasing graph-services
  sbt "release skip-tests default-tag-exists-answer k with-defaults"
}

function updateVersionInRenku() {
  echo "Updating renku-graph version in Renku's requirements.yaml"

  # cloning the renku repo
  cd || exit
  git clone https://github.com/SwissDataScienceCenter/renku.git
  cd renku || exit

  # changing the version
  sed -i "/- name: renku-graph/{n;n;n;s/.*/  version: $TRAVIS_TAG/}" charts/renku/requirements.yaml

  # preparing git config
  git config --global user.name "RenkuBot"
  git config --global user.email "renku@datascience.ch"
  git config credential.helper "store --file=.git/credentials"
  echo "https://${GITHUB_TOKEN}:@github.com" >.git/credentials

  # running helm dep udpate
  cd charts || exit
  helm repo add renku https://swissdatasciencecenter.github.io/helm-charts/
  helm dep update renku
  cd .. || exit

  # pushing to the remote
  git checkout -b auto-update/renku-graph-$TRAVIS_TAG
  git add charts/renku/requirements.yaml
  git commit -m "chore: updating renku-graph version to $TRAVIS_TAG"
  git push --set-upstream origin auto-update/renku-graph-$TRAVIS_TAG
}
