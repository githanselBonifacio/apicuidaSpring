package co.com.sura.web.farmacia;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.farmacia.FarmaciaUseCase;
import co.com.sura.genericos.Response;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

@RestController
@CrossOrigin(value = "http://localhost:4200")
@RequestMapping("/farmacia")
public class FarmaciaController {

    @Autowired
    private FarmaciaUseCase farmaciaUseCase;
    //farmacia
    @GetMapping(value = "tratamientosFarmacia")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmacia(){
        return farmaciaUseCase.consultarAllTratamientosToFarmacia()
                .collectList()
                .map(pacientes -> ResponseFactory.createStatus(
                        pacientes,
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
    @GetMapping(value = "tratamientosFarmaciaWithFilter")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmaciaWithFilter(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idRegional,
            @RequestParam Integer idHorarioTurno
    ){
        return farmaciaUseCase.consultarAllTratamientosToFarmaciaWithFilter(fechaTurno,idRegional,idHorarioTurno)
                .collectList()
                .map(pacientes -> ResponseFactory.createStatus(
                        pacientes,
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

    @PostMapping(value = "notificarFarmacia")
    public Mono<Response<Boolean>>notificarMedicamentosToFarmacia(
            @RequestBody List<PacienteTratamientoCita> tratamientoCitasList){
        return farmaciaUseCase.notificarMedicamentosToFarmacia(tratamientoCitasList)
                .map(seNotifico -> ResponseFactory.createStatus(
                        seNotifico,
                        StatusCode.STATUS_200,
                        Mensajes.SE_NOTIFICO_FARMACIA,
                        Mensajes.SE_NOTIFICO_FARMACIA,
                        Mensajes.PETICION_EXITOSA
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500,
                        Mensajes.NO_NOTIFICO_FARMACIA,
                        Mensajes.PETICION_FALLIDA,
                        e.getMessage()
                )));
    }

}
