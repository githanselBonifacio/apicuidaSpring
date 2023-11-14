package co.com.sura.web.reportes;

import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.entity.reportes.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoAnual;
import co.com.sura.entity.reportes.turnos.ReporteTurnoMensual;
import co.com.sura.reportes.ReportesUseCase;
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
    public Mono<ReporteTurnoAnual> consultarReporteAnual(
            @RequestParam Integer anio,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteAnual(anio,idRegional);
    }

    @GetMapping("/turno/mensual")
    public Mono<ReporteTurnoMensual> consultarReporteMensual(
            @RequestParam Integer anio,
            @RequestParam Integer numeroMes,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteMes(anio,numeroMes,idRegional);
    }
    @GetMapping("/cancelacionCitas/anual")
    public Mono<ReporteCancelacionCitaAnual> consultarReporteCancelacionCitaAnual(
            @RequestParam Integer anio,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultaReporteCancelacionCitasAnual(anio,idRegional);
    }

    @GetMapping("/cancelacionCitas/mensual")
    public Mono<ReporteCancelacionCitaMensual> consultarReporteCancelacionCitaMensual(
            @RequestParam Integer anio,
            @RequestParam Integer numeroMes,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional);
    }
}
