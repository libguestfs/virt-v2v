# -*- python -*-
# oVirt or RHV upload create VM used by ‘virt-v2v -o rhv-upload’
# Copyright (C) 2018 Red Hat Inc.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

import json
import logging
import sys
import time
import uuid

from urllib.parse import urlparse, urlunparse

import ovirtsdk4 as sdk
import ovirtsdk4.types as types


def debug(s):
    if params['verbose']:
        print(s, file=sys.stderr)
        sys.stderr.flush()


def jobs_completed(system_service, correlation_id):
    jobs_service = system_service.jobs_service()

    try:
        jobs = jobs_service.list(
            search="correlation_id=%s" % correlation_id)
    except sdk.Error as e:
        debug(
            "Error searching for jobs with correlation id %s: %s" %
            (correlation_id, e))
        # We don't know, assume that jobs did not complete yet.
        return False

    # STARTED is the only "in progress" status, anything else means the job
    # has already terminated.
    if all(job.status != types.JobStatus.STARTED for job in jobs):
        failed_jobs = [(job.description, str(job.status))
                       for job in jobs
                       if job.status != types.JobStatus.FINISHED]
        if failed_jobs:
            raise RuntimeError(
                "Failed to create a VM! Failed jobs: %r" % failed_jobs)
        return True
    else:
        running_jobs = [(job.description, str(job.status)) for job in jobs]
        debug("Some jobs with correlation id %s are running: %s" %
              (correlation_id, running_jobs))
        return False


# Seconds to wait for the VM import job to complete in oVirt.
timeout = 3 * 60

# Parameters are passed in via a JSON doc from the OCaml code.
# Because this Python code ships embedded inside virt-v2v there
# is no formal API here.
params = None
ovf = None                      # OVF file

if len(sys.argv) != 3:
    raise RuntimeError("incorrect number of parameters")

# Parameters are passed in via a JSON document.
with open(sys.argv[1], 'r') as fp:
    params = json.load(fp)

# What is passed in is a password file, read the actual password.
with open(params['output_password'], 'r') as fp:
    output_password = fp.read()
output_password = output_password.rstrip()

# Read the OVF document.
with open(sys.argv[2], 'r') as fp:
    ovf = fp.read()

# Parse out the username from the output_conn URL.
parsed = urlparse(params['output_conn'])
username = parsed.username or "admin@internal"
netloc = f"{parsed.hostname:parsed.port}" if parsed.port else parsed.hostname

# Connect to the server.
connection = sdk.Connection(
    url=urlunparse(parsed._replace(netloc=netloc)),
    username=username,
    password=output_password,
    ca_file=params['rhv_cafile'],
    log=logging.getLogger(),
    insecure=params['insecure'],
)

system_service = connection.system_service()

# Get the cluster.
cluster = system_service.clusters_service().cluster_service(params['rhv_cluster_uuid'])
cluster = cluster.get()

correlation_id = str(uuid.uuid4())
vms_service = system_service.vms_service()
vm = vms_service.add(
    types.Vm(
        cluster=cluster,
        initialization=types.Initialization(
            configuration=types.Configuration(
                type=types.ConfigurationType.OVA,
                data=ovf,
            )
        )
    ),
    query={'correlation_id': correlation_id},
)

# Wait for the import job to finish.
endt = time.monotonic() + timeout
while True:
    time.sleep(10)
    if jobs_completed(system_service, correlation_id):
        break
    if time.monotonic() > endt:
        raise RuntimeError(
            "Timed out waiting for VM creation!"
            " Jobs still running for correlation id %s" % correlation_id)
