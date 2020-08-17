DM_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
WO_DIR=/opt/choice-survey
DK_IMG=choice-survey-dev-env

while :
	do
	echo "1. Build image"
	echo "2. Enter container"
	echo "3. Execute container"
	echo "4. Generate survey design"
	echo "5. Deploy survey"
	echo "6. Analyse survey data"
	echo "7. Modify configuration"
	echo "8. EXIT"
	echo -n "Choose one option [1 - 8]: "
	read opcion

function build_dev_image () {
	docker build -t "${DK_IMG}" "${DM_DIR}"/Docker/
}

function enter_dev_env () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" /bin/bash
}

function execute () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" /bin/bash "${WO_DIR}"/utils.sh run
}

function generate_design () {
	Rscript generate_design.R
}

function deploy_survey () {
	Rscript deploy_survey.R
}

function analyse_data () {
	Rscript analyse_data.R
}

function modify_conf () {
	sudo nano ./conf.json
}

case $opcion in
	1)
		build_dev_image
		;;
	2)
		enter_dev_env
		;;
	3)
		execute
		;;
	4)
		generate_design
		;;
	5)
		deploy_survey
		;;
	6)
		analyse_data
		;;
	7)
		modify_conf
		;;
	8)
		echo "bye";
		exit 1
		;;
esac
done