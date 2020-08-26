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
	docker run -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" /bin/bash "${WO_DIR}"/utils.sh run
}

function generate_design () {
	docker run --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" Rscript "${WO_DIR}"/generate_design.R run
}

function deploy_survey () {
	docker run -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" Rscript "${WO_DIR}"/deploy_survey.R run
}

function modify_conf () {
	sudo nano ./conf/conf.json
}

function analyse_data () {
	while :
		do
		echo "1. Analyse adaptive survey data"
		echo "2. Analyse external google forms data (Choice)"
		echo "3. Analyse external google forms data (Saaty Multicriteria)"
		echo "4. RETURN"
		echo -n "Choose one option [1 - 4]: "
		read caso
	
	case $caso in
		1)
			docker run -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" Rscript "${WO_DIR}"/analyse_data.R
			;;
		2)
			sudo nano ./conf/google_forms_conf.json
			docker run -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" Rscript "${WO_DIR}"/analyse_googleforms_data.R
			;;
		3)
			sudo nano ./conf/multicriteria_conf.json
			docker run -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 --rm -it -v  "${DM_DIR}":"${WO_DIR}" --network host "${DK_IMG}" Rscript "${WO_DIR}"/saaty_multicriteria.R
			;;
		4)
			echo 'Return';
			break
			;;
	esac
	done
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