package co.com.sura.remision.factory;

import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.personal.entity.SecuenciaTurno;
import co.com.sura.remision.entity.datosremision.ItemDiaTurno;
import co.com.sura.remision.entity.datosremision.ItemSecuenciaTurno;

import java.util.Collections;
import java.util.List;

public interface RemisionFactory {
     static ItemDiaTurno crearItemDiaTurno(
            Integer numeroDia, String nombreDia, List<HorarioTurno> horariosTurno){
        return  ItemDiaTurno.builder()
                .numeroDia(numeroDia)
                .nombreDia(nombreDia)
                .horariosTurno(horariosTurno)
                .build();
    }

     static ItemDiaTurno crearItemDiaTurno(ItemSecuenciaTurno itemSecuenciaTurno){
        return ItemDiaTurno.builder()
                .numeroDia(itemSecuenciaTurno.getNumeroDia())
                .nombreDia(itemSecuenciaTurno.getNombreDia())
                .horariosTurno(itemSecuenciaTurno.getHorariosTurno())
                .build();
    }

     static SecuenciaTurno crearSecuenciaTurno(ItemSecuenciaTurno itemSecuenciaTurno, ItemDiaTurno itemsDiaTurno){
        return SecuenciaTurno.builder()
                .nombre(itemSecuenciaTurno.getNombreSecuencia())
                .descripcion(itemSecuenciaTurno.getDescripcion())
                .itemsDiaTurno(Collections.singletonList(itemsDiaTurno))
                .build();
    }

}
