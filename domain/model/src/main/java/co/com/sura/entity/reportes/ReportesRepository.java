package co.com.sura.entity.reportes;

import reactor.core.publisher.Mono;

public interface ReportesRepository {

    Mono<ReporteTurnoAnual> consultarReporteAnual (Integer anio, String idRegional);
    Mono<ReporteTurnoMes> consultarReporteMensual (Integer anio,Integer numeroMes, String idRegional);
}
