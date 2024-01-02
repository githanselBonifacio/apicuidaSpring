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
import org.springframework.web.bind.annotation.*;

import reactor.core.publisher.Mono;

@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/reportes")
public class ReportesController {

    @Autowired
    private ReportesUseCase reportesUseCase;

    /**
     * consultar reporte de turno anual
     * @param anio año de consulta (Integer)
     * @param idRegional  id regional de consulta (String)
     * @return reporte anual (Response<ReporteTurnoAnual>)
     * */
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

    /**
     * consultar reporte de turno mensual
     * @param anio año de consulta (Integer)
     * @param numeroMes numero del mes consultado (Integer)
     * @param idRegional  id regional de consulta (String)
     * @return reporte mensual (Response<ReporteTurnoMensual>)
     * */
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
    /**
     * consultar reporte de cancelación citas anual
     * @param anio año de consulta (Integer)
     * @param idRegional  id regional de consulta (String)
     * @return reporte mensual (Response<ReporteCancelacionCitaAnual>)
     * */
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

    /**
     * consultar reporte de cancelación citas mensual
     * @param anio año de consulta (Integer)
     * @param numeroMes numero del mes consultado (Integer)
     * @param idRegional  id regional de consulta (String)
     * @return reporte mensual (Response<ReporteCancelacionCitaMensual>)
     * */
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
