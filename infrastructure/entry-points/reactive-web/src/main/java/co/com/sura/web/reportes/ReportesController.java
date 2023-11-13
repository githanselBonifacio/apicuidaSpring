package co.com.sura.web.reportes;

import co.com.sura.entity.reportes.ReporteTurnoAnual;
import co.com.sura.entity.reportes.ReporteTurnoMes;
import co.com.sura.moviles.reportes.ReportesUseCase;
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

    @GetMapping("/anual")
    public Mono<ReporteTurnoAnual> consultarReporteAnual(
            @RequestParam Integer anio,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteAnual(anio,idRegional);
    }

    @GetMapping("/mensual")
    public Mono<ReporteTurnoMes> consultarReporteMensual(
            @RequestParam Integer anio,
            @RequestParam Integer numeroMes,
            @RequestParam  String idRegional){
        return  reportesUseCase.consultarReporteMes(anio,numeroMes,idRegional);
    }
}
