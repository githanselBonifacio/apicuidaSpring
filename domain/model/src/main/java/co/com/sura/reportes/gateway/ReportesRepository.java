package co.com.sura.reportes.gateway;

import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import reactor.core.publisher.Mono;

public interface ReportesRepository {

    Mono<ReporteTurnoAnual> consultarReporteAnual (Integer anio, String idRegional);
    Mono<ReporteTurnoMensual> consultarReporteMensual (Integer anio, Integer numeroMes, String idRegional);
    Mono<ReporteCancelacionCitaAnual> consultaReporteCancelacionCitasAnual(Integer anio, String idRegional);
    Mono<ReporteCancelacionCitaMensual> consultaReporteCancelacionCitasMensual (
            Integer anio, Integer numeroMes, String idRegional);
}
