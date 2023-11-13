package co.com.sura.moviles.reportes;

import co.com.sura.entity.reportes.ReporteTurnoAnual;
import co.com.sura.entity.reportes.ReporteTurnoMes;
import co.com.sura.entity.reportes.ReportesRepository;
import reactor.core.publisher.Mono;


public class ReportesUseCase {
    private final ReportesRepository reportesRepository;

    public ReportesUseCase(ReportesRepository reportesRepository) {
        this.reportesRepository = reportesRepository;
    }

    public Mono<ReporteTurnoAnual> consultarReporteAnual(Integer anio, String idRegional){
        return reportesRepository.consultarReporteAnual(anio, idRegional);
    }

    public Mono<ReporteTurnoMes> consultarReporteMes(Integer anio, Integer numeroMes, String idRegional){
        return reportesRepository.consultarReporteMensual(anio,numeroMes, idRegional);
    }
}
