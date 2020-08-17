DM_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
WO_DIR=/opt/global-sentinel
DK_IMG=global-sentinel-dev-env

function build_dev_image () {
	docker build -t "${DK_IMG}" "${DM_DIR}"/Docker/
}

function enter_dev_env () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" /bin/bash
}

function execute () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" /bin/bash "${WO_DIR}"/utils.sh run
}

function run () {
    # entrypoint
    echo "run"
}

case $1 in
	build)
		build_dev_image
		;;
	enter)
		enter_dev_env
		;;
	execute_container)
		execute
		;;
	run)
		run $2
		;;
esac