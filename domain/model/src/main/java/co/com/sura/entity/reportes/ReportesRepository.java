package co.com.sura.entity.reportes;

import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoAnual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoMensual;
import reactor.core.publisher.Mono;

public interface ReportesRepository {

    Mono<ReporteTurnoAnual> consultarReporteAnual (Integer anio, String idRegional);
    Mono<ReporteTurnoMensual> consultarReporteMensual (Integer anio, Integer numeroMes, String idRegional);
    Mono<ReporteCancelacionCitaAnual> consultaReporteCancelacionCitasAnual(Integer anio, String idRegional);
    Mono<ReporteCancelacionCitaMensual> consultaReporteCancelacionCitasMensual (
            Integer anio, Integer numeroMes, String idRegional);
}
