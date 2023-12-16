package co.com.sura.web.reportes;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import co.com.sura.genericos.Response;
import co.com.sura.reportes.ReportesUseCase;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import reactor.core.publisher.Mono;

@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/reportes")
public class ReportesController {

    @Autowired
    private ReportesUseCase reportesUseCase;

    @GetMapping("/turno/anual")
    public Mono<Response<ReporteTurnoAnual>> consultarReporteAnual(
            @RequestParam Integer anio,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteAnual(anio,idRegional)
                .map(reporte -> ResponseFactory.createStatus(
                        reporte,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    @GetMapping("/turno/mensual")
    public Mono<Response<ReporteTurnoMensual>> consultarReporteMensual(
            @RequestParam Integer anio,
            @RequestParam Integer numeroMes,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteMes(anio,numeroMes,idRegional)
                .map(reporte -> ResponseFactory.createStatus(
                        reporte,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }
    @GetMapping("/cancelacionCitas/anual")
    public Mono<Response<ReporteCancelacionCitaAnual>> consultarReporteCancelacionCitaAnual(
            @RequestParam Integer anio,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultaReporteCancelacionCitasAnual(anio,idRegional)
         .map(reporte -> ResponseFactory.createStatus(
                 reporte,
                StatusCode.STATUS_200,
                Mensajes.PETICION_EXITOSA,
                Mensajes.PETICION_EXITOSA,
                Mensajes.PETICION_EXITOSA
        ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

    @GetMapping("/cancelacionCitas/mensual")
    public Mono<Response<ReporteCancelacionCitaMensual>> consultarReporteCancelacionCitaMensual(
            @RequestParam Integer anio,
            @RequestParam Integer numeroMes,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional)
                .map(reporte -> ResponseFactory.createStatus(
                        reporte,
                        StatusCode.STATUS_200,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.PETICION_FALLIDA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

}
