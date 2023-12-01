package co.com.sura.web.farmacia;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.farmacia.FarmaciaUseCase;
import co.com.sura.genericos.Response;
import co.com.sura.web.factory.ResponseFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

@RestController
@CrossOrigin(origins = "*")
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
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }
    @GetMapping(value = "tratamientosFarmaciaWithFilter")
    public Mono<Response<List<PacienteTratamientoCita>>> consultarMedicamentosToFarmaciaWithFilter(
            @RequestParam("fechaTurno") @DateTimeFormat(pattern = "yyyy-MM-dd") LocalDate fechaTurno,
            @RequestParam String idRegional,
            @RequestParam Integer idHorarioTurno
    ){
        return farmaciaUseCase.consultarAllTratamientosToFarmaciaWithFilter(fechaTurno,idHorarioTurno,idRegional)
                .collectList()
                .map(pacientes -> ResponseFactory.createStatus(
                        pacientes,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

    @PostMapping(value = "notificarFarmacia")
    public Mono<Response<Boolean>>notificarMedicamentosToFarmacia(
            @RequestBody List<PacienteTratamientoCita> tratamientoCitasList){
        return farmaciaUseCase.notificarMedicamentosToFarmacia(tratamientoCitasList)
                .map(seNotifico -> ResponseFactory.createStatus(
                        seNotifico,
                        StatusCode.STATUS_200.getValue(),
                        Mensajes.SE_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.SE_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.PETICION_EXITOSA.getValue()
                ))
                .onErrorResume(e -> Mono.just(ResponseFactory.createStatus(
                        null,
                        StatusCode.STATUS_500.getValue(),
                        Mensajes.NO_NOTIFICO_FARMACIA.getValue(),
                        Mensajes.PETICION_FALLIDA.getValue(),
                        e.getMessage()
                )));
    }

}
