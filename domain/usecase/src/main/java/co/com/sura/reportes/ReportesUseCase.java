package co.com.sura.reportes;

import co.com.sura.reportes.gateway.ReportesRepository;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import reactor.core.publisher.Mono;


public class ReportesUseCase {
    private final ReportesRepository reportesRepository;

    public ReportesUseCase(ReportesRepository reportesRepository) {
        this.reportesRepository = reportesRepository;
    }

    public Mono<ReporteTurnoAnual> consultarReporteAnual(Integer anio, String idRegional){
        return reportesRepository.consultarReporteAnual(anio, idRegional);
    }

    public Mono<ReporteTurnoMensual> consultarReporteMes(Integer anio, Integer numeroMes, String idRegional){
        return reportesRepository.consultarReporteMensual(anio,numeroMes, idRegional);
    }

    public Mono<ReporteCancelacionCitaAnual> consultaReporteCancelacionCitasAnual(Integer anio, String idRegional){
        return reportesRepository.consultaReporteCancelacionCitasAnual(anio,idRegional);
    }
    public Mono<ReporteCancelacionCitaMensual> consultaReporteCancelacionCitasMensual(
            Integer anio,Integer mes, String idRegional){
        return reportesRepository.consultaReporteCancelacionCitasMensual(anio,mes,idRegional);
    }

}
